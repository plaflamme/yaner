use std::cell::Cell;
use std::pin::Pin;
use crate::cartridge::Cartridge;
use crate::memory::AddressSpace;
use bitflags::_core::ops::Add;
use std::ops::{Generator, GeneratorState};

pub struct Nes {
    ram: crate::memory::Ram2KB,
    cartridge: crate::cartridge::Cartridge,
    cpu: crate::cpu::Cpu,
    // TODO: ppu
    // TODO: apu
    // TODO: input
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        Nes {
            ram: crate::memory::Ram2KB::new(),
            cartridge,
            cpu: crate::cpu::Cpu::new()
        }
    }

    pub fn run(&self) {
        let boxed: Box<&dyn AddressSpace> = Box::new(self);
        let mut cpu_ticks = self.cpu.run(&boxed);
        match Pin::new(&mut cpu_ticks).resume() {
            GeneratorState::Yielded(crate::cpu::CpuCycle::Tick) => println!("Tick!"),
            GeneratorState::Yielded(crate::cpu::CpuCycle::Done { op, mode }) => println!("Done {:?}, {:?}", op, mode),
            GeneratorState::Complete(_) => unimplemented!()
        };
    }
}

impl crate::memory::AddressSpace for Nes {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2000..=0x2007 => unimplemented!(), // PPU
            0x2008..=0x3FFF => unimplemented!(), // PPU mirror

            0x4000..=0x4017 => unimplemented!(), // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.cartridge.mapper.read_u8(addr), // PRG ROM/RAM and mapper
            _ => panic!("invalid address 0x{:X?}", addr)
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x8000, value),

            0x2000..=0x2007 => unimplemented!(), // PPU
            0x2008..=0x3FFF => unimplemented!(), // PPU mirror

            0x4000..=0x4017 => unimplemented!(), // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.cartridge.mapper.write_u8(addr, value), // PRG ROM/RAM and mapper
            _ => panic!("invalid address 0x{:X?}", addr)
        }
    }
}
