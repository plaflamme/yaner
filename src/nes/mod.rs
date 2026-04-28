use std::cell::{Cell, RefCell};
use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::rc::Rc;

use ouroboros::self_referencing;

use crate::Reset;
use crate::cartridge::{Cartridge, Mapper};
use crate::cpu::RP2A03;
use crate::input::{Input, Joypad};
use crate::memory::{AddressSpace, Ram2KB};
use crate::ppu::{Ppu, PpuCycle, PpuRegisters};

pub mod debug;

type NesCoroutine<'a> = impl Coroutine<Yield = NesCycle, Return = !> + Unpin + 'a;

#[derive(Default)]
pub struct Clocks {
    pub ppu_master_clock: Cell<u64>,
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

pub struct Bus {
    pub ram: Ram2KB,
    pub ppu_registers: PpuRegisters,
    pub mapper: Rc<RefCell<Box<dyn Mapper>>>,
    pub input_1: Rc<dyn Input>,
    pub input_2: Rc<dyn Input>,
}

impl Bus {
    fn new(
        ppu_registers: PpuRegisters,
        mapper: Rc<RefCell<Box<dyn Mapper>>>,
        input_1: Rc<dyn Input>,
        input_2: Rc<dyn Input>,
    ) -> Self {
        Self {
            ram: Ram2KB::new(),
            ppu_registers,
            mapper,
            input_1,
            input_2,
        }
    }

    fn decay_open_bus(&self) {
        self.ppu_registers.decay_open_bus();
    }
}

impl AddressSpace for Bus {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2000..=0x2007 => self.ppu_registers.read_u8(addr), // PPU
            0x2008..=0x3FFF => self.ppu_registers.read_u8(0x2000 + (addr % 8)), // PPU mirror

            // In the NES and Famicom, the top three (or five) bits are not driven, and so retain the bits of the previous byte on the bus.
            // Usually this is the most significant byte of the address of the controller port—0x40.
            // Certain games (such as Paperboy) rely on this behavior and require that reads from the controller ports return exactly $40 or $41 as appropriate.
            0x4016 => self.input_1.read() | 0x40, // joy1
            0x4017 => self.input_2.read() | 0x40, // joy2

            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            // Mapper
            _ => self.mapper.borrow().read_u8(addr), // PRG ROM/RAM and mapper
        }
    }
    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x0800, value),

            0x2000..=0x2007 => self.ppu_registers.write_u8(addr, value), // PPU
            0x2008..=0x3FFF => self.ppu_registers.write_u8(0x2000 + (addr % 8), value), // PPU mirror

            0x4016 => {
                // The first bit is connected to the inputs
                // TODO: is this supposed to happen now or on the next tick?
                let out0 = value & 0x01;
                self.input_1.strobe(out0);
                self.input_2.strobe(out0);
            }
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            // Mapper
            _ => self.mapper.borrow().write_u8(addr, value), // PRG ROM/RAM and mapper
        }
    }
}

pub struct Nes {
    pub cpu: RP2A03<Bus>,
    pub ppu: Rc<Ppu>,
    pub clocks: Clocks,
    pub input1: Rc<Joypad>, // TODO: abstract these away (dyn Input) and use Option
    pub input2: Rc<Joypad>,
    pub mapper: Rc<RefCell<Box<dyn crate::cartridge::Mapper>>>,
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
        let ppu_registers = PpuRegisters::new(ppu.clone());
        let bus = Bus::new(
            ppu_registers,
            mapper.clone(),
            input1.clone(),
            input2.clone(),
        );
        let cpu = RP2A03::new(start_at, bus);

        Nes {
            cpu,
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
        // force CUP/PPU alignment to this non-random value:
        // * avoids the special case where cpu/ppu are aligned
        // * avoids non-determinism
        let ppu_offset = 1;

        macro_rules! tick_ppu {
            () => {
                while self.clocks.ppu_master_clock.get() + ppu_stride
                    <= (self.cpu.clock.get() - ppu_offset)
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
                        self.cpu.bus.decay_open_bus()
                    }
                }
            };
        }

        #[coroutine]
        move || {
            yield NesCycle::PowerUp;
            loop {
                match Pin::new(&mut cpu).resume(()) {
                    CoroutineState::Yielded(cycle) => {
                        if cycle.is_cycle_end() {
                            self.clocks.tick_cpu();
                        }
                        yield NesCycle::Cpu(cycle);
                        tick_ppu!();
                    }
                    CoroutineState::Complete(_) => panic!("cpu stopped"),
                };
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
