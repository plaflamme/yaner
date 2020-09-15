use std::cell::{Cell, RefCell};
use std::fmt::{Display, Formatter};
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

use dma::DmaCycle;

use crate::cartridge::Cartridge;
use crate::cpu::{Cpu, CpuBus, CpuCycle, Interrupt};
use crate::nes::dma::Dma;
use crate::ppu::{MemoryMappedRegisters, Ppu, PpuCycle};
use std::borrow::BorrowMut;
use std::error::Error;

pub mod debug;
mod dma;

pub struct Clocks {
    pub cpu_cycles: Cell<u64>,
    pub ppu_cycles: Cell<u64>,
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
        }
    }

    fn tick(&self, with_cpu_cycle: bool) {
        self.ppu_cycles.update(|c| c.wrapping_add(1));
        if with_cpu_cycle {
            self.cpu_cycles.update(|c| c.wrapping_add(1));
        }
    }

    pub fn ppu_frame(&self) -> u64 {
        self.ppu_cycles.get() / 341
    }

    pub fn ppu_dot(&self) -> u16 {
        (self.ppu_cycles.get() % 341) as u16
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
    // TODO: apu
    // TODO: input
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        let mapper = Rc::new(RefCell::new(cartridge.mapper));
        let ppu = Rc::new(Ppu::new(mapper.clone()));
        let ppu_mem_registers = MemoryMappedRegisters::new(ppu.clone());
        let cpu_bus = CpuBus::new(ppu_mem_registers, mapper.clone());

        Nes {
            cpu: Cpu::new(cpu_bus),
            ppu,
            dma: Dma::new(),
            clocks: Clocks::new(),
        }
    }

    pub fn debug(&self) -> debug::NesState {
        debug::NesState::new(self)
    }

    // yields on every nes ppu tick
    pub fn ppu_steps(
        &self,
        start_at: Option<u16>,
    ) -> impl Generator<Yield = NesCycle, Return = ()> + '_ {
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
                            _ => (),
                        }
                        cycle
                    }
                    GeneratorState::Complete(_) => panic!("ppu stopped"),
                };

                let mut dma_step = || match Pin::new(&mut dma).resume(()) {
                    GeneratorState::Yielded(DmaCycle::NoDma) => true,
                    GeneratorState::Yielded(DmaCycle::Tick) => true,
                    GeneratorState::Yielded(DmaCycle::Done) => false,
                    GeneratorState::Complete(_) => true,
                };

                if let Some(addr) = self.cpu.bus.dma_latch() {
                    self.dma.start(addr, self.clocks.cpu_cycles.get());
                    cpu_suspended = true;
                }

                if !cpu_suspended && self.clocks.ppu_cycles.get() % 3 == 0 {
                    match Pin::new(&mut cpu).resume(()) {
                        GeneratorState::Yielded(CpuCycle::Halt) => break,
                        GeneratorState::Yielded(cpu_cycle) => {
                            self.clocks.tick(true);
                            yield NesCycle::CpuCycle(cpu_cycle, ppu_step())
                        }
                        GeneratorState::Complete(_) => panic!("cpu stopped"),
                    }
                } else {
                    if cpu_suspended {
                        cpu_suspended = dma_step();
                    }
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
    pub fn run(&self, start_at: Option<u16>) {
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

pub struct Stepper<'a> {
    nes: &'a Nes,
    steps: Box<dyn Generator<Yield = NesCycle, Return = ()> + Unpin + 'a>,
    halted: bool,
}

impl<'a> Stepper<'a> {
    // TODO: normally, this should consume `Nes`, but this requires more refactoring
    pub fn new(nes: &'a Nes, start_at: Option<u16>) -> Self {
        Stepper {
            nes,
            steps: Box::new(nes.ppu_steps(start_at)),
            halted: false,
        }
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
                self.steps.borrow_mut();
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
