use std::cell::{Cell, RefCell};
use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::rc::Rc;

use ouroboros::self_referencing;

use crate::Reset;
use crate::cartridge::Cartridge;
use crate::cpu::{CpuBus, IoRegisters};
use crate::input::Joypad;
use crate::memory::AddressSpace;
use crate::ppu::{Ppu, PpuCycle, PpuRegisters};

pub mod debug;

type NesCoroutine<'a> = impl Coroutine<Yield = NesCycle, Return = ()> + Unpin + 'a;

#[derive(Default)]
pub struct Clocks {
    pub cpu_master_clock: Cell<usize>,
    pub ppu_master_clock: Cell<usize>,
    pub cpu_cycles: Cell<u64>,
    pub ppu_cycles: Cell<u64>,
    pub ppu_frames: Cell<u64>,
}

impl Clocks {
    fn tick_cpu(&self) {
        self.cpu_cycles.update(|c| c.wrapping_add(1));
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
    Cpu(yaner_cpu::CpuEvent),
    Ppu(PpuCycle),
}

pub struct Nes {
    pub cpu: yaner_cpu::Cpu,
    pub cpu_bus: CpuBus,
    pub ppu: Rc<Ppu>,
    pub clocks: Clocks,
    pub input1: Rc<Joypad>, // TODO: abstract these away (dyn Input) and use Option
    pub input2: Rc<Joypad>,
    pub mapper: Rc<RefCell<Box<dyn crate::cartridge::Mapper>>>,
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
            cpu: yaner_cpu::Cpu::new(start_at),
            cpu_bus,
            ppu,
            clocks: Clocks::default(),
            input1,
            input2,
            mapper,
        }
    }

    pub fn debug(&self) -> debug::NesState<'_> {
        debug::NesState::new(self)
    }

    // yields on every nes ppu tick
    #[define_opaque(NesCoroutine)]
    fn run(&self) -> NesCoroutine<'_> {
        let mut cpu = self.cpu.run();
        let mut ppu = self.ppu.run();
        let ppu_stride = 4;

        // From what I understand, the PPU detects 0 -> 1 transitions on the master clock while
        // the cpu detects 1 -> 0 transitions.
        // This effectively means that they're always off by one master clock tick which is represented here.
        let ppu_offset = 0;

        self.clocks.cpu_master_clock.set(12);
        #[coroutine]
        move || {
            yield NesCycle::PowerUp;
            loop {
                match Pin::new(&mut cpu).resume(()) {
                    CoroutineState::Yielded(
                        cycle @ yaner_cpu::CpuEvent::HalfCycle {
                            phase: yaner_cpu::Phase::One,
                            rw,
                            addr,
                        },
                    ) => match rw {
                        yaner_cpu::Rw::Read => {
                            let value = self.cpu_bus.read_u8(addr);
                            self.clocks.cpu_master_clock.update(|c| c + 5);
                            self.cpu.io_bus.set(value);
                            yield NesCycle::Cpu(cycle);
                        }
                        yaner_cpu::Rw::Write => {
                            self.clocks.cpu_master_clock.update(|c| c + 7);
                            self.cpu_bus.write_u8(addr, self.cpu.io_bus.get());
                            yield NesCycle::Cpu(cycle);
                        }
                    },
                    CoroutineState::Yielded(
                        cycle @ yaner_cpu::CpuEvent::HalfCycle {
                            phase: yaner_cpu::Phase::Two,
                            rw,
                            addr: _,
                        },
                    ) => match rw {
                        yaner_cpu::Rw::Read => {
                            self.clocks.cpu_master_clock.update(|c| c + 7);
                            self.clocks.tick_cpu();
                            yield NesCycle::Cpu(cycle);
                        }
                        yaner_cpu::Rw::Write => {
                            self.clocks.cpu_master_clock.update(|c| c + 5);
                            self.clocks.tick_cpu();
                            yield NesCycle::Cpu(cycle);
                        }
                    },

                    CoroutineState::Complete(_) => panic!("cpu stopped"),
                };

                if let Some(addr) = self.cpu_bus.io_regsiters.dma_latch() {
                    self.cpu.dma_latch.set(Some(addr));
                }

                while self.clocks.ppu_master_clock.get() + ppu_stride
                    <= (self.clocks.cpu_master_clock.get() - ppu_offset)
                {
                    match Pin::new(&mut ppu).resume(()) {
                        CoroutineState::Yielded(cycle) => {
                            match cycle {
                                PpuCycle::Tick { nmi } => {
                                    self.cpu.set_nmi(nmi);
                                }
                                PpuCycle::Frame => self.clocks.tick_frame(),
                            }
                            self.clocks.tick_ppu();
                            self.clocks.ppu_master_clock.update(|c| c + ppu_stride);
                            yield NesCycle::Ppu(cycle)
                        }
                        CoroutineState::Complete(_) => panic!("ppu stopped"),
                    };

                    // According to ppu_open_bus/readme.txt, the open bus register should decay
                    //   to 0 if a bit hasn't been set to 1 in the last ~600ms.
                    //
                    // The PPU runs at 21.477272 MHz / 4 (5.369318 Mhz)
                    //   5_369_318 cycles/s
                    //   0.6 * 5_369_318 = 3_221_590.8
                    //   So 600ms on the NES is approximately 3_221_590 PPU ticks
                    if self.clocks.ppu_cycles.get().is_multiple_of(3_221_590) {
                        // TODO: this should remember when each bit was last set to 1
                        // TODO: this should just happen as a side effect of ticking the ppu
                        self.cpu_bus.ppu_registers.decay_open_bus()
                    }
                }
            }
        }
    }

    pub fn steps(self) -> Steps {
        StepsBuilder {
            nes: self,
            halted: false,
            steps_builder: |nes: &Nes| nes.run(),
        }
        .build()
    }
}

impl Reset for Nes {
    fn reset(&self) {
        self.cpu.reset();
    }
}

#[self_referencing]
pub struct Steps {
    nes: Nes,
    #[borrows(nes)]
    #[not_covariant]
    steps: NesCoroutine<'this>,
    halted: bool,
}

impl Steps {
    pub fn nes(&self) -> &Nes {
        self.borrow_nes()
    }

    pub fn halted(&self) -> bool {
        *self.borrow_halted()
    }

    pub fn step(&mut self) -> Result<NesCycle, StepperError> {
        if self.halted() {
            Err(StepperError::Halted)
        } else {
            let cycle = self.with_steps_mut(|s| Pin::new(s).resume(()));
            match cycle {
                CoroutineState::Yielded(cycle) => Ok(cycle),
                CoroutineState::Complete(_) => Err(StepperError::Halted),
            }
        }
    }

    pub fn run(&mut self) -> Result<(), StepperError> {
        loop {
            self.step()?;
            if self.halted() {
                break Ok(());
            }
        }
    }

    pub fn step_until(
        &mut self,
        mut stop: impl FnMut(&Nes, NesCycle) -> bool,
    ) -> Result<(), StepperError> {
        loop {
            let cycle = self.step()?;
            if stop(self.nes(), cycle) {
                break Ok(());
            }
        }
    }

    pub fn step_frame(&mut self) -> Result<PpuCycle, StepperError> {
        loop {
            if let NesCycle::Ppu(PpuCycle::Frame) = self.step()? {
                break Ok(PpuCycle::Frame);
            }
        }
    }

    pub fn step_ppu(&mut self) -> Result<PpuCycle, StepperError> {
        loop {
            if let NesCycle::Ppu(cycle) = self.step()? {
                break Ok(cycle);
            }
        }
    }

    pub fn step_cpu(&mut self) -> Result<yaner_cpu::CpuEvent, StepperError> {
        let active_pc = self.nes().cpu.active_pc();
        loop {
            if let NesCycle::Cpu(cycle) = self.step()?
                && self.nes().cpu.active_pc() != active_pc
            {
                break Ok(cycle);
            }
        }
    }
}

impl Reset for Steps {
    fn reset(&self) {
        self.nes().reset()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum StepperError {
    Halted,
}

impl std::fmt::Display for StepperError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StepperError::Halted => write!(f, "NES has already halted, cannot step"),
        }
    }
}

impl std::error::Error for StepperError {}
