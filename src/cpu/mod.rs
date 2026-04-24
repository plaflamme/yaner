#![allow(non_camel_case_types)]

use std::ops::{Coroutine, CoroutineState};
use yaner_cpu::Cpu;
use yaner_cpu::CpuEvent;

use crate::apu::Apu;
use crate::apu::ApuCycle;
use crate::cartridge::Mapper;
use crate::input::Input;
use crate::memory::{AddressSpace, Ram2KB};
use crate::ppu::PpuRegisters;
use std::cell::{Cell, RefCell};
use std::pin::Pin;
use std::rc::Rc;

pub struct RP2A03 {
    pub(crate) cycles: Cell<u64>,
    pub(crate) apu_cycles: Cell<u64>, // TODO: remove this?
    pub(crate) cpu: yaner_cpu::Cpu,
    pub(crate) apu: Rc<Apu>,
    pub(crate) cpu_bus: CpuBus,
}

impl RP2A03 {
    pub fn new(
        start_at: Option<u16>,
        mapper: Rc<RefCell<Box<dyn Mapper>>>,
        ppu_registers: PpuRegisters,
        input_1: Rc<dyn Input>,
        input_2: Rc<dyn Input>,
    ) -> Self {
        let apu = Rc::new(Apu::default());
        let io_registers = IoRegisters::new(apu.clone(), input_1, input_2);
        let cpu_bus = CpuBus::new(io_registers, ppu_registers, mapper);
        Self {
            cycles: Cell::new(12),
            apu_cycles: Cell::default(),
            cpu: Cpu::new(start_at),
            apu,
            cpu_bus,
        }
    }

    pub fn set_nmi(&self, state: bool) {
        self.cpu.set_nmi(state);
    }

    pub fn decay_open_bus(&self) {
        self.cpu_bus.ppu_registers.decay_open_bus();
    }

    pub(crate) fn active_pc(&self) -> u16 {
        self.cpu.active_pc()
    }

    pub(crate) fn reset(&self) {
        self.cpu.reset();
    }

    pub fn run(&self) -> impl Coroutine<Yield = CpuEvent, Return = ()> + '_ {
        #[coroutine]
        || {
            let apu_stride = 12;
            let mut cpu = self.cpu.run();
            let mut apu = self.apu.run();
            macro_rules! tick_apu {
                () => {
                    while self.apu_cycles.get() + apu_stride < self.cycles.get() {
                        match Pin::new(&mut apu).resume(()) {
                            CoroutineState::Yielded(ApuCycle::Tick { irq }) => {
                                if irq {
                                    self.cpu.set_irq(irq);
                                }
                            }
                            CoroutineState::Complete(_) => (),
                        }
                        self.apu_cycles.update(|c| c + apu_stride);
                    }
                };
            }
            loop {
                {
                    match Pin::new(&mut cpu).resume(()) {
                        CoroutineState::Yielded(
                            cycle @ yaner_cpu::CpuEvent::HalfCycle {
                                phase: yaner_cpu::Phase::One,
                                rw,
                                addr,
                            },
                        ) => match rw {
                            yaner_cpu::Rw::Read => {
                                self.cycles.update(|c| c + 5);
                                tick_apu!();
                                yield cycle;

                                let value = self.cpu_bus.read_u8(addr);
                                self.cpu.io_bus.set(value);
                            }
                            yaner_cpu::Rw::Write => {
                                self.cycles.update(|c| c + 7);
                                tick_apu!();
                                yield cycle;
                                self.cpu_bus.write_u8(addr, self.cpu.io_bus.get());
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
                                self.cycles.update(|c| c + 7);
                                tick_apu!();
                                yield cycle;
                            }
                            yaner_cpu::Rw::Write => {
                                self.cycles.update(|c| c + 5);
                                tick_apu!();
                                yield cycle;
                            }
                        },

                        CoroutineState::Complete(_) => panic!("cpu stopped"),
                    };
                    if let Some(addr) = self.cpu_bus.io_regsiters.dma_latch() {
                        self.cpu.dma_latch.set(Some(addr));
                    }
                };
            }
        }
    }
}

pub struct CpuBus {
    pub ram: Ram2KB,
    pub io_regsiters: IoRegisters,
    pub ppu_registers: PpuRegisters,
    pub mapper: Rc<RefCell<Box<dyn Mapper>>>,
}

impl CpuBus {
    pub fn new(
        io_regsiters: IoRegisters,
        ppu_registers: PpuRegisters,
        mapper: Rc<RefCell<Box<dyn Mapper>>>,
    ) -> Self {
        CpuBus {
            ram: Ram2KB::default(),
            io_regsiters,
            ppu_registers,
            mapper,
        }
    }
}

impl crate::memory::AddressSpace for CpuBus {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2000..=0x2007 => self.ppu_registers.read_u8(addr), // PPU
            0x2008..=0x3FFF => self.ppu_registers.read_u8(0x2000 + (addr % 8)), // PPU mirror

            0x4000..=0x401F => self.io_regsiters.read_u8(addr),

            0x4020..=0xFFFF => self.mapper.borrow().read_u8(addr), // PRG ROM/RAM and mapper
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x0800, value),

            0x2000..=0x2007 => self.ppu_registers.write_u8(addr, value), // PPU
            0x2008..=0x3FFF => self.ppu_registers.write_u8(0x2000 + (addr % 8), value), // PPU mirror

            0x4000..=0x401F => self.io_regsiters.write_u8(addr, value),

            0x4020..=0xFFFF => self.mapper.borrow().write_u8(addr, value), // PRG ROM/RAM and mapper
        }
    }
}

// http://wiki.nesdev.org/w/index.php/2A03
pub struct IoRegisters {
    apu: Rc<Apu>,
    input1: Rc<dyn Input>,
    input2: Rc<dyn Input>,

    // OUT0-OUT2 latch
    out_latch: Cell<u8>,

    dma_latch: Cell<Option<u8>>,
}

impl IoRegisters {
    pub fn new(apu: Rc<Apu>, input1: Rc<dyn Input>, input2: Rc<dyn Input>) -> Self {
        IoRegisters {
            apu,
            input1,
            input2,
            out_latch: Cell::default(),
            dma_latch: Cell::new(None),
        }
    }

    // This will return last write to OAM DMA and then None until the next write
    pub fn dma_latch(&self) -> Option<u8> {
        self.dma_latch.take()
    }
}

impl AddressSpace for IoRegisters {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x4015 => self.apu.read_u8(addr),
            // In the NES and Famicom, the top three (or five) bits are not driven, and so retain the bits of the previous byte on the bus.
            // Usually this is the most significant byte of the address of the controller port—0x40.
            // Certain games (such as Paperboy) rely on this behavior and require that reads from the controller ports return exactly $40 or $41 as appropriate.
            0x4016 => self.input1.read() | 0x40, // joy1
            0x4017 => self.input2.read() | 0x40, // joy2

            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.
            _ => 0x0,
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x4014 => {
                log::debug!("DMA@0x{value:02X}");
                self.dma_latch.set(Some(value))
            }
            0x4016 => {
                self.out_latch.set(value & 0x7); // lower 3 bits

                // The first bit is connected to the inputs
                // TODO: is this supposed to happen now or on the next tick?
                let out0 = value & 0x01;
                self.input1.strobe(out0);
                self.input2.strobe(out0);
            }
            0x4000..=0x4013 | 0x4015 | 0x4017 => self.apu.write_u8(addr, value),
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.
            _ => (),
        }
    }
}
