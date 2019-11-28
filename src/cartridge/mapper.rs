use super::Mapper;
use super::rom::Rom;
use crate::memory::AddressSpace;

pub struct Unknown();

impl Mapper for Unknown {
    fn name(&self) -> String {
        "Unknown".to_string()
    }
}

impl AddressSpace for Unknown {
    fn read_u8(&self, _addr: u16) -> u8 {
        unimplemented!()
    }

    fn write_u8(&self, _addr: u16, _value: u8) {
        unimplemented!()
    }
}

// http://wiki.nesdev.com/w/index.php/NROM
pub struct NROM {
    prg_ram: crate::memory::Ram2KB,
    prg_rom: Vec<u8>,
}

impl From<&Rom> for NROM {
    fn from(rom: &Rom) -> Self {
        NROM {
            prg_ram: crate::memory::Ram2KB::new(),
            prg_rom: rom.prg_rom.clone()
        }
    }
}

impl Mapper for NROM {
    fn name(&self) -> String {
        "NROM".to_string()
    }
}

impl AddressSpace for NROM {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            // PRG RAM,
            0x6000..=0x7FFF => self.prg_ram.read_u8(addr - 0x6000),
            // 0x8000..=0xBFFF - First 16 KB of ROM.
            // 0xC000..=0xFFFF - Last 16 KB of ROM (NROM-256) or mirror of $8000-$BFFF (NROM-128).
            0x8000..=0xFFFF => self.prg_rom[(addr - 0x8000) as usize % self.prg_rom.len()],
            _ => panic!("addressed an unmapped location 0x{:X?}", addr)
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x6000..=0x7FFF => self.prg_ram.write_u8(addr - 0x6000, value),
            0x8000..=0xFFFF => panic!("tried writing to a read only location 0x{:X?}", addr),
            _ => panic!("addressed an unmapped location 0x{:X?}", addr)
        }
    }
}
