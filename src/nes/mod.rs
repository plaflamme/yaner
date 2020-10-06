use std::cell::{Cell, RefCell};
use std::fmt::{Display, Formatter};
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

use dma::DmaCycle;

use crate::cartridge::Cartridge;
use crate::cpu::{Cpu, CpuBus, CpuCycle, Interrupt, IoRegisters};
use crate::input::Joypad;
use crate::nes::dma::Dma;
use crate::ppu::{Ppu, PpuCycle, PpuRegisters};
use std::borrow::BorrowMut;
use std::error::Error;

pub mod debug;
mod dma;

pub struct Clocks {
    pub cpu_cycles: Cell<u64>,
    pub ppu_cycles: Cell<u64>,
    pub ppu_frames: Cell<u64>,
}

impl Clocks {
    fn new() -> Self {
        // start at 7 due to reset interrupt handling
        //   See start sequence here http://users.telenet.be/kim1-6502/6502/proman.html#92
        let cpu_cycles = 7u64;
        // starts at 21, probably for the same reason as above, but this doesn't seem to be documented anywhere
        let ppu_cycles = 21u64;
        Clocks {
            cpu_cycles: Cell::new(cpu_cycles),
            ppu_cycles: Cell::new(ppu_cycles),
            ppu_frames: Cell::default(),
        }
    }

    fn tick(&self, with_cpu_cycle: bool) {
        self.ppu_cycles.update(|c| c.wrapping_add(1));
        if with_cpu_cycle {
            self.cpu_cycles.update(|c| c.wrapping_add(1));
        }
    }

    fn tick_frame(&self) {
        self.ppu_frames.update(|c| c.wrapping_add(1));
    }
}

#[derive(Debug)]
pub enum NesCycle {
    PowerUp,
    PpuCycle(PpuCycle),
    CpuCycle(CpuCycle, PpuCycle),
}

pub struct Nes {
    pub cpu: Cpu,
    pub ppu: Rc<Ppu>,
    dma: Dma,
    pub clocks: Clocks,
    pub input1: Rc<Joypad>, // TODO: abstract these away (dyn Input) and use Option
    pub input2: Rc<Joypad>,
    // TODO: apu
    // TODO: input
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        let input1 = Rc::new(crate::input::Joypad::default());
        let input2 = Rc::new(crate::input::Joypad::default());
        let mapper = Rc::new(RefCell::new(cartridge.mapper));
        let ppu = Rc::new(Ppu::new(mapper.clone()));
        let ppu_mem_registers = PpuRegisters::new(ppu.clone());
        let io_registers = IoRegisters::new(input1.clone(), input2.clone());
        let cpu_bus = CpuBus::new(io_registers, ppu_mem_registers, mapper.clone());

        Nes {
            cpu: Cpu::new(cpu_bus),
            ppu,
            dma: Dma::new(),
            clocks: Clocks::new(),
            input1,
            input2,
        }
    }

    pub fn debug(&self) -> debug::NesState {
        debug::NesState::new(self)
    }

    // yields on every nes ppu tick
    pub fn ppu_steps<'a>(
        &'a self,
        start_at: Option<u16>,
    ) -> impl Generator<Yield = NesCycle, Return = ()> + 'a {
        let mut cpu_suspended = false;
        let mut cpu = self.cpu.run(start_at);
        let mut ppu = self.ppu.run();
        let mut dma = self.dma.run(&self.cpu.bus);

        move || {
            yield NesCycle::PowerUp;
            loop {
                let mut ppu_step = || match Pin::new(&mut ppu).resume(()) {
                    GeneratorState::Yielded(cycle) => {
                        match cycle {
                            PpuCycle::Nmi => self.cpu.bus.intr.set(Some(Interrupt::Nmi)),
                            PpuCycle::Frame => self.clocks.tick_frame(),
                            _ => (),
                        }
                        cycle
                    }
                    GeneratorState::Complete(_) => panic!("ppu stopped"),
                };

                let mut dma_step = || match Pin::new(&mut dma).resume(()) {
                    GeneratorState::Yielded(DmaCycle::NoDma) => false,
                    GeneratorState::Yielded(DmaCycle::Tick) => true,
                    GeneratorState::Yielded(DmaCycle::Done) => false,
                    GeneratorState::Complete(_) => panic!("dma stopped"),
                };

                if let Some(addr) = self.cpu.bus.io_regsiters.dma_latch() {
                    self.dma.start(addr, self.clocks.cpu_cycles.get());
                    cpu_suspended = true;
                }

                if self.clocks.ppu_cycles.get() % 3 == 0 {
                    if !cpu_suspended {
                        match Pin::new(&mut cpu).resume(()) {
                            GeneratorState::Yielded(CpuCycle::Halt) => break,
                            GeneratorState::Yielded(cpu_cycle) => {
                                self.clocks.tick(true);
                                yield NesCycle::CpuCycle(cpu_cycle, ppu_step())
                            }
                            GeneratorState::Complete(_) => panic!("cpu stopped"),
                        }
                    } else {
                        cpu_suspended = dma_step();
                        self.clocks.tick(false);
                        yield NesCycle::PpuCycle(ppu_step())
                    }
                } else {
                    self.clocks.tick(false);
                    yield NesCycle::PpuCycle(ppu_step());
                }

                if self.clocks.ppu_cycles.get() % 10_000 == 0 {
                    // TODO: this should be ~100ms
                    // TODO: this should just happen as a side effect of ticking the ppu
                    self.cpu.bus.ppu_registers.decay_open_bus()
                }
            }
        }
    }

    // runs the program until the CPU halts
    pub fn run(self, start_at: Option<u16>) {
        let mut stepper = Stepper::new(self, start_at);
        loop {
            match stepper.step_frame() {
                Ok(_) => (),
                Err(StepperError::Halted) => break,
            }
        }
    }
}

impl Display for Nes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.cpu)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum StepperError {
    Halted,
}

impl Display for StepperError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StepperError::Halted => write!(f, "NES has already halted, cannot step"),
        }
    }
}

impl Error for StepperError {}

pub struct Stepper {
    nes: Nes,
    steps: Option<Box<dyn Generator<Yield = NesCycle, Return = ()> + Unpin + 'static>>,
    halted: bool,
}

impl Stepper {
    pub fn new(nes: Nes, start_at: Option<u16>) -> Pin<Box<Stepper>> {
        let s = Stepper {
            nes,
            steps: None,
            halted: false,
        };

        // NOTE: we use pin to fix the address of `s.nes`
        let mut pinned = Box::pin(s);

        // here be dragons...
        unsafe {
            // NOTE: we extend the lifetime of the generator.
            //   This is necessary because I can't figure out how to tell / convince Rust that the reference to `boxed.nes` has a lifetime that is >= the generator's lifetime
            //   I have no idea how to avoid this and transmute is the most unsafe thing you can do...
            let generator = std::mem::transmute::<
                Box<dyn Generator<Yield = NesCycle, Return = ()> + Unpin + '_>,
                Box<dyn Generator<Yield = NesCycle, Return = ()> + Unpin + 'static>,
            >(Box::new(pinned.nes.ppu_steps(start_at)));

            // Here we do the typical self-referencial struct trick described in Pin
            let mut_ref: Pin<&mut Self> = Pin::as_mut(&mut pinned);
            Pin::get_unchecked_mut(mut_ref).steps = Some(generator);
        };
        pinned
    }

    pub fn nes(&self) -> &Nes {
        &self.nes
    }

    pub fn halted(&self) -> bool {
        self.halted
    }

    pub fn tick(&mut self) -> Result<NesCycle, StepperError> {
        if self.halted {
            Err(StepperError::Halted)
        } else {
            // NOTE: we have to help the compiler here by using type ascription
            let gen: &mut (dyn Generator<Yield = NesCycle, Return = ()> + Unpin) =
                self.steps.as_mut().unwrap().borrow_mut();
            match Pin::new(gen).resume(()) {
                GeneratorState::Yielded(cycle @ NesCycle::CpuCycle(CpuCycle::Halt, _)) => {
                    self.halted = true;
                    Ok(cycle)
                }
                GeneratorState::Yielded(cycle) => Ok(cycle),
                GeneratorState::Complete(_) => Err(StepperError::Halted),
            }
        }
    }

    pub fn tick_until(&mut self, mut stop: impl FnMut(&Nes) -> bool) -> Result<(), StepperError> {
        loop {
            self.tick()?;
            if stop(&self.nes) {
                break Ok(());
            }
        }
    }

    pub fn step_frame(&mut self) -> Result<PpuCycle, StepperError> {
        loop {
            match self.tick()? {
                NesCycle::CpuCycle(_, PpuCycle::Frame) => break Ok(PpuCycle::Frame),
                NesCycle::PpuCycle(PpuCycle::Frame) => break Ok(PpuCycle::Frame),
                _ => (),
            }
        }
    }

    pub fn step_cpu(&mut self) -> Result<CpuCycle, StepperError> {
        loop {
            match self.tick()? {
                NesCycle::CpuCycle(cycle @ CpuCycle::OpComplete(_, _), _) => break Ok(cycle),
                NesCycle::CpuCycle(CpuCycle::Halt, _) => break Ok(CpuCycle::Halt),
                _ => (),
            }
        }
    }
}
