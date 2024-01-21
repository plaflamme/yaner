use std::cell::{Cell, RefCell};
use std::fmt::{Display, Formatter};
use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::rc::Rc;

use crate::cartridge::Cartridge;
use crate::cpu::{Cpu, CpuBus, CpuCycle, Interrupt, IoRegisters};
use crate::input::Joypad;
use crate::ppu::{Ppu, PpuCycle, PpuRegisters};
use crate::Reset;
use std::borrow::BorrowMut;
use std::error::Error;

pub mod debug;

#[derive(Default)]
pub struct Clocks {
    pub cpu_cycles: Cell<u64>,
    pub ppu_cycles: Cell<u64>,
    pub ppu_frames: Cell<u64>,
}

impl Clocks {
    fn tick_cpu(&self) {
        self.cpu_cycles.update(|c| c.wrapping_add(1));
        self.ppu_cycles.update(|c| c.wrapping_add(1));
    }

    fn tick_ppu(&self) {
        self.ppu_cycles.update(|c| c.wrapping_add(1));
    }

    fn tick_frame(&self) {
        self.ppu_frames.update(|c| c.wrapping_add(1));
    }
}

#[derive(Debug)]
pub enum NesCycle {
    PowerUp,
    PpuCycle(PpuCycle),
    CpuCycle(CpuCycle),
}

pub struct Nes {
    pub cpu: Cpu,
    pub ppu: Rc<Ppu>,
    pub clocks: Clocks,
    pub input1: Rc<Joypad>, // TODO: abstract these away (dyn Input) and use Option
    pub input2: Rc<Joypad>,
    // TODO: apu
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        Nes::new_with_pc(cartridge, None)
    }
    pub fn new_with_pc(cartridge: Cartridge, start_at: Option<u16>) -> Self {
        let input1 = Rc::new(crate::input::Joypad::default());
        let input2 = Rc::new(crate::input::Joypad::default());
        let mapper = Rc::new(RefCell::new(cartridge.mapper()));
        let ppu = Rc::new(Ppu::new(mapper.clone()));
        let ppu_mem_registers = PpuRegisters::new(ppu.clone());
        let io_registers = IoRegisters::new(input1.clone(), input2.clone());
        let cpu_bus = CpuBus::new(io_registers, ppu_mem_registers, mapper.clone());

        Nes {
            cpu: Cpu::new(cpu_bus, start_at),
            ppu,
            clocks: Clocks::default(),
            input1,
            input2,
        }
    }

    pub fn debug(&self) -> debug::NesState {
        debug::NesState::new(self)
    }

    // yields on every nes ppu tick
    pub fn ppu_steps(&self) -> impl Coroutine<Yield = NesCycle, Return = ()> + '_ {
        let mut cpu = self.cpu.run();
        let mut ppu = self.ppu.run();
        let ppu_stride = 4;
        let ppu_offset = 1;
        let mut ppu_clock = 0;
        let mut master_clock = 0_usize;

        move || {
            yield NesCycle::PowerUp;
            loop {
                match Pin::new(&mut cpu).resume(()) {
                    CoroutineState::Yielded(cycle) => match cycle {
                        CpuCycle::Phi1(clock_ticks) => {
                            master_clock += clock_ticks;
                        }
                        CpuCycle::Tick(clock_ticks) => {
                            master_clock += clock_ticks;
                            self.clocks.tick_cpu();
                            yield NesCycle::CpuCycle(cycle)
                        }
                        CpuCycle::OpComplete(_, _) => {
                            yield NesCycle::CpuCycle(cycle);
                            continue;
                        }
                        CpuCycle::Halt => break,
                    },
                    CoroutineState::Complete(_) => panic!("cpu stopped"),
                }

                while ppu_clock + ppu_stride + ppu_offset < master_clock {
                    match Pin::new(&mut ppu).resume(()) {
                        CoroutineState::Yielded(cycle) => {
                            match cycle {
                                PpuCycle::Nmi => self.cpu.bus.intr.set(Some(Interrupt::Nmi)),
                                PpuCycle::Frame => self.clocks.tick_frame(),
                                _ => (),
                            }
                            self.clocks.tick_ppu();
                            yield NesCycle::PpuCycle(cycle)
                        }
                        CoroutineState::Complete(_) => panic!("ppu stopped"),
                    };
                    ppu_clock += ppu_stride;

                    // According to ppu_open_bus/readme.txt, the open bus register should decay
                    //   to 0 if a bit hasn't been set to 1 in the last ~600ms.
                    //
                    // The PPU runs at 21.477272 MHz / 4 (5.369318 Mhz)
                    //   5_369_318 cycles/s
                    //   0.6 * 5_369_318 = 3_221_590.8
                    //   So 600ms on the NES is approximately 3_221_590 PPU ticks
                    if self.clocks.ppu_cycles.get() % 3_221_590 == 0 {
                        // TODO: this should remember when each bit was last set to 1
                        // TODO: this should just happen as a side effect of ticking the ppu
                        self.cpu.bus.ppu_registers.decay_open_bus()
                    }
                }
            }
        }
    }

    // runs the program until the CPU halts
    pub fn run(self) {
        let mut stepper = Stepper::new(self);
        while stepper.step_frame().is_ok() {}
    }
}

impl Display for Nes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.cpu)
    }
}

impl Reset for Nes {
    fn reset(&self) {
        self.cpu.reset();
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
    steps: Option<Box<dyn Coroutine<Yield = NesCycle, Return = ()> + Unpin + 'static>>,
    halted: bool,
}

impl Stepper {
    pub fn new(nes: Nes) -> Pin<Box<Stepper>> {
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
                Box<dyn Coroutine<Yield = NesCycle, Return = ()> + Unpin + '_>,
                Box<dyn Coroutine<Yield = NesCycle, Return = ()> + Unpin + 'static>,
            >(Box::new(pinned.nes.ppu_steps()));

            // Here we do the typical self-referential struct trick described in Pin
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

    pub fn step(&mut self) -> Result<NesCycle, StepperError> {
        if self.halted {
            Err(StepperError::Halted)
        } else {
            // NOTE: we have to help the compiler here by using type ascription
            let gen: &mut (dyn Coroutine<Yield = NesCycle, Return = ()> + Unpin) =
                self.steps.as_mut().unwrap().borrow_mut();
            match Pin::new(gen).resume(()) {
                CoroutineState::Yielded(cycle @ NesCycle::CpuCycle(CpuCycle::Halt)) => {
                    self.halted = true;
                    Ok(cycle)
                }
                CoroutineState::Yielded(cycle) => Ok(cycle),
                CoroutineState::Complete(_) => Err(StepperError::Halted),
            }
        }
    }

    pub fn step_until(&mut self, mut stop: impl FnMut(&Nes) -> bool) -> Result<(), StepperError> {
        loop {
            self.step()?;
            if stop(&self.nes) {
                break Ok(());
            }
        }
    }

    pub fn step_frame(&mut self) -> Result<PpuCycle, StepperError> {
        loop {
            match self.step()? {
                NesCycle::PpuCycle(PpuCycle::Frame) => break Ok(PpuCycle::Frame),
                _ => (),
            }
        }
    }

    pub fn step_cpu(&mut self) -> Result<CpuCycle, StepperError> {
        loop {
            match self.step()? {
                NesCycle::CpuCycle(cycle @ CpuCycle::OpComplete(_, _)) => break Ok(cycle),
                NesCycle::CpuCycle(CpuCycle::Halt) => break Ok(CpuCycle::Halt),
                _ => (),
            }
        }
    }
}

impl Reset for Stepper {
    fn reset(&self) {
        self.nes.reset();
    }
}
