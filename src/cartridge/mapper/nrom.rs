use super::{Mapper, NametableMirroring};
use crate::cartridge::mapper::{BankSelect, Switched};
use crate::cartridge::rom::RomData;
use crate::cartridge::rom::{Chr, Rom};
use crate::memory::{AddressSpace, Dyn};

// http://wiki.nesdev.com/w/index.php/NROM
#[allow(clippy::upper_case_acronyms)]
pub struct NROM {
    mirroring: NametableMirroring,
    prg_ram: Dyn,
    prg_rom: Switched<Dyn>,
    chr: Chr,
}

impl From<&Rom> for NROM {
    fn from(rom: &Rom) -> Self {
        let data = RomData::new(rom);
        let prg_rom_size = data.prg_rom.size();
        let prg_rom = Switched::new(data.prg_rom, prg_rom_size, 16_384);
        NROM {
            mirroring: rom.nametable_mirroring,
            prg_ram: data.prg_ram,
            prg_rom,
            chr: data.chr, // wiki says there's no chr_ram, but it seems like some tests assume this is supported anyway
        }
    }
}

impl Mapper for NROM {
    fn name(&self) -> &'static str {
        "NROM"
    }

    fn nametable_mirroring(&self) -> NametableMirroring {
        self.mirroring
    }
}

impl AddressSpace for NROM {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.chr.read_u8(addr),
            // PRG RAM,
            0x6000..=0x7FFF => self.prg_ram.read_u8(addr - 0x6000),
            // 0x8000..=0xBFFF - First 16 KB of ROM.
            0x8000..=0xBFFF => self.prg_rom.read_u8(BankSelect::First, addr - 0x8000),
            // 0xC000..=0xFFFF - Last 16 KB of ROM (NROM-256) or mirror of $8000-$BFFF (NROM-128).
            0xC000..=0xFFFF => self.prg_rom.read_u8(BankSelect::Last, addr - 0xC000),
            _ => invalid_address!(addr),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => self.chr.write_u8(addr, value),
            0x6000..=0x7FFF => self.prg_ram.write_u8(addr - 0x6000, value),
            0x8000..=0xBFFF => self
                .prg_rom
                .write_u8(BankSelect::First, addr - 0x8000, value),
            0xC000..=0xFFFF => self
                .prg_rom
                .write_u8(BankSelect::Last, addr - 0xC000, value),
            _ => invalid_address!(addr),
        }
    }
}
