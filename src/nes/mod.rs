use std::cell::Cell;

struct Nes {
    ram: crate::memory::Ram2KB,
    cartridge: crate::cartridge::Cartridge,
    cpu: crate::cpu::RP2A03,
    // TODO: ppu
    // TODO: apu
    // TODO: input
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

            0x4020..=0xFFFF => unimplemented!(), // PRG ROM/RAM and mapper
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

            0x4020..=0xFFFF => unimplemented!(), // PRG ROM/RAM and mapper
            _ => panic!("invalid address 0x{:X?}", addr)
        }
    }
}
