use std::cell::{Cell, RefCell};
use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::rc::Rc;

use ouroboros::self_referencing;

use crate::cartridge::Cartridge;
use crate::cpu::{Cpu, CpuBus, CpuCycle, IoRegisters};
use crate::input::Joypad;
use crate::ppu::{Ppu, PpuCycle, PpuRegisters};
use crate::Reset;

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
    // NOTE: CpuCycle::OpComplete is yielded once the CPU/PPU are aligned
    Cpu(CpuCycle),
    Ppu(PpuCycle),
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
    fn ppu_steps(&self) -> NesCoroutine {
        let mut cpu = self.cpu.run();
        let mut ppu = self.ppu.run();
        let ppu_stride = 4;

        // From what I understand, the PPU detects 0 -> 1 transitions on the master clock while
        // the cpu detects 1 -> 0 transitions.
        // This effectively means that they're always off by one master clock tick which is represented here.
        let ppu_offset = 1;

        self.clocks.cpu_master_clock.set(12);

        move || {
            yield NesCycle::PowerUp;
            loop {
                let cpu_tick = match Pin::new(&mut cpu).resume(()) {
                    CoroutineState::Yielded(cycle) => match cycle {
                        CpuCycle::Phi1(clock_ticks) => {
                            self.clocks.cpu_master_clock.update(|c| c + clock_ticks);
                            yield NesCycle::Cpu(cycle);
                            None
                        }
                        CpuCycle::Tick(clock_ticks) => {
                            self.clocks.cpu_master_clock.update(|c| c + clock_ticks);
                            self.clocks.tick_cpu();
                            yield NesCycle::Cpu(cycle);
                            None
                        }
                        CpuCycle::OpComplete(_, _) => Some(cycle),
                        CpuCycle::Halt => break,
                    },
                    CoroutineState::Complete(_) => panic!("cpu stopped"),
                };

                while self.clocks.ppu_master_clock.get() + ppu_stride
                    <= (self.clocks.cpu_master_clock.get() - ppu_offset)
                {
                    match Pin::new(&mut ppu).resume(()) {
                        CoroutineState::Yielded(cycle) => {
                            match cycle {
                                PpuCycle::Tick { nmi } => self.cpu.bus.set_nmi_line(nmi),
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
                    if self.clocks.ppu_cycles.get() % 3_221_590 == 0 {
                        // TODO: this should remember when each bit was last set to 1
                        // TODO: this should just happen as a side effect of ticking the ppu
                        self.cpu.bus.ppu_registers.decay_open_bus()
                    }
                }

                // When we tick the CPU, the PPU hasn't caught up yet.
                // So in order for the consumer to know when this alignment occurs, we yield OpComplete
                //   after the ppu has caught up. OpComplete, isn't a "real" CPU tick.
                if let Some(op @ CpuCycle::OpComplete(_, _)) = cpu_tick {
                    yield NesCycle::Cpu(op)
                }
            }
        }
    }

    pub fn steps(self) -> Steps {
        StepsBuilder {
            nes: self,
            halted: false,
            steps_builder: |nes: &Nes| nes.ppu_steps(),
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
                CoroutineState::Yielded(cycle @ NesCycle::Cpu(CpuCycle::Halt)) => {
                    self.with_halted_mut(|h| *h = true);
                    Ok(cycle)
                }
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
            match self.step()? {
                NesCycle::Ppu(cycle) => break Ok(cycle),
                NesCycle::Cpu(CpuCycle::Halt) => break Err(StepperError::Halted),
                _ => (),
            }
        }
    }

    pub fn step_cpu(&mut self) -> Result<CpuCycle, StepperError> {
        loop {
            match self.step()? {
                NesCycle::Cpu(cycle @ CpuCycle::OpComplete(_, _)) => break Ok(cycle),
                NesCycle::Cpu(CpuCycle::Halt) => break Ok(CpuCycle::Halt),
                _ => (),
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
