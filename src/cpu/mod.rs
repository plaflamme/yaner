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
    pub(crate) bus: CpuBus,
}

impl RP2A03 {
    pub fn new(
        start_at: Option<u16>,
        mapper: Rc<RefCell<Box<dyn Mapper>>>,
        ppu_registers: PpuRegisters,
        input_1: Rc<dyn Input>,
        input_2: Rc<dyn Input>,
    ) -> Self {
        Self {
            cycles: Cell::new(12),
            apu_cycles: Cell::default(),
            bus: CpuBus::new(
                Cpu::new(start_at),
                Apu::default(),
                ppu_registers,
                mapper,
                input_1,
                input_2,
            ),
        }
    }

    pub fn set_nmi(&self, state: bool) {
        self.bus.cpu.set_nmi(state);
    }

    pub fn decay_open_bus(&self) {
        self.bus.ppu_registers.decay_open_bus();
    }

    pub(crate) fn active_pc(&self) -> u16 {
        self.bus.cpu.active_pc()
    }

    pub(crate) fn reset(&self) {
        self.bus.cpu.reset();
    }

    pub fn run(&self) -> impl Coroutine<Yield = CpuEvent, Return = ()> + '_ {
        #[coroutine]
        || {
            let apu_stride = 12;
            let mut cpu = self.bus.cpu.run();
            let mut apu = self.bus.apu.run();
            macro_rules! tick_apu {
                () => {
                    while self.apu_cycles.get() + apu_stride < self.cycles.get() {
                        match Pin::new(&mut apu).resume(()) {
                            CoroutineState::Yielded(ApuCycle::Tick { irq }) => {
                                if irq {
                                    self.bus.cpu.set_irq(irq);
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
                                self.bus.cpu_read_u8(addr);
                            }
                            yaner_cpu::Rw::Write => {
                                self.cycles.update(|c| c + 7);
                                tick_apu!();
                                yield cycle;
                                self.bus.cpu_write_u8(addr);
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
                };
            }
        }
    }
}

pub struct CpuBus {
    pub cpu: Cpu,
    pub apu: Apu,
    pub ram: Ram2KB,
    pub ppu_registers: PpuRegisters,
    pub mapper: Rc<RefCell<Box<dyn Mapper>>>,
    pub input_1: Rc<dyn Input>,
    pub input_2: Rc<dyn Input>,
}

impl CpuBus {
    pub fn new(
        cpu: Cpu,
        apu: Apu,
        ppu_registers: PpuRegisters,
        mapper: Rc<RefCell<Box<dyn Mapper>>>,
        input_1: Rc<dyn Input>,
        input_2: Rc<dyn Input>,
    ) -> Self {
        Self {
            cpu,
            apu,
            ram: Ram2KB::default(),
            ppu_registers,
            mapper,
            input_1,
            input_2,
        }
    }

    // Read value from addr and write it to the CPU IO bus.
    fn cpu_read_u8(&self, addr: u16) {
        self.cpu.io_bus.set(self.read_u8(addr));
    }

    // Write the value from the CPU's IO bus to addr.
    fn cpu_write_u8(&self, addr: u16) {
        self.write_u8(addr, self.cpu.io_bus.get());
    }
}

impl crate::memory::AddressSpace for CpuBus {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2000..=0x2007 => self.ppu_registers.read_u8(addr), // PPU
            0x2008..=0x3FFF => self.ppu_registers.read_u8(0x2000 + (addr % 8)), // PPU mirror

            // "IO registers"
            0x4000..=0x4014 => invalid_address!(addr, 0x00),

            0x4015 => self.apu.read_u8(addr),
            // In the NES and Famicom, the top three (or five) bits are not driven, and so retain the bits of the previous byte on the bus.
            // Usually this is the most significant byte of the address of the controller port—0x40.
            // Certain games (such as Paperboy) rely on this behavior and require that reads from the controller ports return exactly $40 or $41 as appropriate.
            0x4016 => self.input_1.read() | 0x40, // joy1
            0x4017 => self.input_2.read() | 0x40, // joy2

            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            // Mapper
            0x4020..=0xFFFF => self.mapper.borrow().read_u8(addr), // PRG ROM/RAM and mapper
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x0800, value),

            0x2000..=0x2007 => self.ppu_registers.write_u8(addr, value), // PPU
            0x2008..=0x3FFF => self.ppu_registers.write_u8(0x2000 + (addr % 8), value), // PPU mirror

            // "IO registers"
            0x4014 => {
                log::debug!("DMA@0x{value:02X}");
                self.cpu.dma_latch.set(Some(value))
            }
            0x4016 => {
                // The first bit is connected to the inputs
                // TODO: is this supposed to happen now or on the next tick?
                let out0 = value & 0x01;
                self.input_1.strobe(out0);
                self.input_2.strobe(out0);
            }
            0x4000..=0x4013 | 0x4015 | 0x4017 => self.apu.write_u8(addr, value),
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            // Mapper
            0x4020..=0xFFFF => self.mapper.borrow().write_u8(addr, value), // PRG ROM/RAM and mapper
        }
    }
}
