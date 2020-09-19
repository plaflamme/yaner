use super::{Mapper, NametableMirroring};
use crate::cartridge::rom::Rom;
use crate::cartridge::rom::RomData;
use crate::memory::AddressSpace;

// http://wiki.nesdev.com/w/index.php/NROM
pub struct NROM {
    mirroring: NametableMirroring,
    data: RomData,
}

impl From<&Rom> for NROM {
    fn from(rom: &Rom) -> Self {
        NROM {
            mirroring: rom.nametable_mirroring,
            data: RomData::new(&rom),
        }
    }
}

impl Mapper for NROM {
    fn name(&self) -> String {
        "NROM".to_string()
    }

    fn nametable_mirroring(&self) -> NametableMirroring { self.mirroring }

    fn cpu_addr_space(&self) -> &dyn AddressSpace {
        self // TODO
    }

    fn ppu_addr_space(&self) -> &dyn AddressSpace {
        self // TODO
    }
}

impl AddressSpace for NROM {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.data.chr.read_u8(addr),
            // PRG RAM,
            0x6000..=0x7FFF => self.data.prg_ram.read_u8(addr - 0x6000),
            // 0x8000..=0xBFFF - First 16 KB of ROM.
            // 0xC000..=0xFFFF - Last 16 KB of ROM (NROM-256) or mirror of $8000-$BFFF (NROM-128).
            0x8000..=0xFFFF => {
                self.data.prg_rom[(addr - 0x8000) as usize % self.data.prg_rom.len()]
            }
            _ => invalid_address!(addr),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => self.data.chr.write_u8(addr, value),
            0x6000..=0x7FFF => self.data.prg_ram.write_u8(addr - 0x6000, value),
            0x8000..=0xFFFF => panic!("tried writing to a read only location 0x{:X?}", addr),
            _ => invalid_address!(addr),
        }
    }
}
