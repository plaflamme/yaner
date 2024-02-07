#![allow(non_camel_case_types)]

use crate::apu::Apu;
use crate::cartridge::Mapper;
use crate::input::Input;
use crate::memory::{AddressSpace, Ram2KB};
use crate::ppu::PpuRegisters;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

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

// http://wiki.nesdev.com/w/index.php/2A03
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
            0x4014 => self.dma_latch.set(Some(value)),
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
