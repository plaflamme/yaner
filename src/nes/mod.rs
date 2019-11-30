use std::pin::Pin;
use crate::cartridge::Cartridge;
use crate::memory::AddressSpace;
use std::ops::{Generator, GeneratorState};

pub struct Nes {
    ram: crate::memory::Ram2KB,
    cartridge: crate::cartridge::Cartridge,
    cpu: crate::cpu::Cpu,
    ppu: crate::ppu::Ppu,
    // TODO: apu
    // TODO: input
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        Nes {
            ram: crate::memory::Ram2KB::new(),
            cartridge,
            cpu: crate::cpu::Cpu::new(),
            ppu: crate::ppu::Ppu::new(),
        }
    }

    pub fn run(&self, start_at: Option<u16>) {
        let boxed: Box<&dyn AddressSpace> = Box::new(self);
        let mut cycle = 7u32; // start at 7 due to reset interrupt handling
        let mut cpu_cycles = self.cpu.run(&boxed, start_at);

        trace!("{} CYC:{}", self.cpu.write(&boxed), cycle);
        loop {
            match Pin::new(&mut cpu_cycles).resume() {
                GeneratorState::Yielded(crate::cpu::CpuCycle::Tick) => cycle = cycle + 1,
                GeneratorState::Yielded(crate::cpu::CpuCycle::OpComplete { pc: _, op: _, mode: _ }) => {
                    trace!("{} CYC:{}", self.cpu.write(&boxed), cycle);
                },
                GeneratorState::Yielded(crate::cpu::CpuCycle::Halt) => {
                    trace!("HALT");
                    break;
                },
                GeneratorState::Complete(_) => unimplemented!()
            };
        }
    }
}

impl crate::memory::AddressSpace for Nes {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2002 => self.ppu.status(),
            0x2000..=0x2007 => unimplemented!(), // PPU
            0x2008..=0x3FFF => unimplemented!(), // PPU mirror

            0x4000..=0x4017 => unimplemented!(), // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.cartridge.mapper.read_u8(addr), // PRG ROM/RAM and mapper
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x8000, value),

            0x2000..=0x2007 => unimplemented!(), // PPU
            0x2008..=0x3FFF => unimplemented!(), // PPU mirror

            0x4000..=0x4017 => (), // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.cartridge.mapper.write_u8(addr, value), // PRG ROM/RAM and mapper
        }
    }
}
