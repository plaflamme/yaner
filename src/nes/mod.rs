use std::pin::Pin;
use crate::cartridge::Cartridge;
use crate::memory::{AddressSpace, Ram2KB};
use crate::cpu::Cpu;
use crate::ppu::Ppu;
use std::ops::{Generator, GeneratorState};

pub struct Nes {
    ram: Ram2KB,
    cartridge: Cartridge,
    cpu: Cpu,
    ppu: Ppu,
    // TODO: apu
    // TODO: input
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        Nes {
            ram: Ram2KB::new(),
            cartridge,
            cpu: Cpu::new(),
            ppu: Ppu::new(),
        }
    }

    pub fn run(&self, start_at: Option<u16>) {
        let cpu_addr_space = CpuAddressSpace::new(self);

        let mut cycle = 7u32; // start at 7 due to reset interrupt handling
        let mut cpu_cycles = self.cpu.run(&cpu_addr_space, start_at);

        trace!("{} CYC:{}", self.cpu.write(&cpu_addr_space), cycle);
        loop {
            match Pin::new(&mut cpu_cycles).resume() {
                GeneratorState::Yielded(crate::cpu::CpuCycle::Tick) => cycle = cycle + 1,
                GeneratorState::Yielded(crate::cpu::CpuCycle::OpComplete { pc: _, op: _, mode: _ }) => {
                    trace!("{} CYC:{}", self.cpu.write(&cpu_addr_space), cycle);
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

struct CpuAddressSpace<'a> {
    ram: &'a dyn AddressSpace,
    ppu: &'a dyn AddressSpace,
    mapper: &'a dyn AddressSpace,
}

impl<'a> CpuAddressSpace<'a> {
    fn new(nes: &'a Nes) -> Self {
        CpuAddressSpace {
            ram: &nes.ram,
            ppu: &nes.ppu,
            mapper: nes.cartridge.mapper.as_addr_space(),
        }
    }
}

impl<'a> crate::memory::AddressSpace for CpuAddressSpace<'a> {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2000..=0x2007 => self.ppu.read_u8(addr), // PPU
            0x2008..=0x3FFF => self.ppu.read_u8(0x2000 + (addr % 8)), // PPU mirror

            0x4000..=0x4017 => unimplemented!(), // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.mapper.read_u8(addr), // PRG ROM/RAM and mapper
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x8000, value),

            0x2000..=0x2007 => self.ppu.write_u8(addr, value), // PPU
            0x2008..=0x3FFF => self.ppu.write_u8(0x2000 + (addr % 8), value), // PPU mirror

            0x4000..=0x4017 => (), // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.mapper.write_u8(addr, value), // PRG ROM/RAM and mapper
        }
    }
}
