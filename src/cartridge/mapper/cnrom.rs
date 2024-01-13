use crate::cartridge::mapper::{BankSelect, Switched};
use crate::cartridge::rom::{Chr, Rom, RomData};
use crate::cartridge::{Mapper, NametableMirroring};
use crate::memory::{AddressSpace, Dyn};
use std::cell::Cell;

#[allow(clippy::upper_case_acronyms)]
pub struct CNROM {
    mirroring: NametableMirroring,
    prg_rom: Switched<Dyn>,
    chr: Switched<Dyn>,
    chr_select: Cell<u8>,
}

impl From<&Rom> for CNROM {
    fn from(rom: &Rom) -> Self {
        let data = RomData::new(rom);
        let chr_rom = match data.chr {
            Chr::Rom(chr_data) => Dyn::new(chr_data),
            Chr::Ram(_) => panic!("CNROM doesn't support CHR RAM"), // TODO: we should use TryFrom now that these can fail
        };
        CNROM {
            mirroring: rom.nametable_mirroring,
            prg_rom: Switched::new(data.prg_rom.clone(), data.prg_rom.len(), 16_384),
            chr: Switched::new(chr_rom, 32_768, 8_192),
            chr_select: Cell::default(),
        }
    }
}

impl Mapper for CNROM {
    fn name(&self) -> &'static str {
        "CNROM"
    }

    fn nametable_mirroring(&self) -> NametableMirroring {
        self.mirroring
    }

    fn cpu_addr_space(&self) -> &dyn AddressSpace {
        self
    }

    fn ppu_addr_space(&self) -> &dyn AddressSpace {
        self
    }
}

impl AddressSpace for CNROM {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => {
                let idx = BankSelect::Index(self.chr_select.get());
                self.chr.read_u8(idx, addr)
            }

            // PRG RAM => None
            0x6000..=0x7FFF => 0x00,

            // 0x8000..=0xBFFF - First 16 KB of ROM.
            0x8000..=0xBFFF => self.prg_rom.read_u8(BankSelect::First, addr - 0x8000),
            // 0xC000..=0xFFFF - Last 16 KB of ROM
            0xC000..=0xFFFF => self.prg_rom.read_u8(BankSelect::Last, addr - 0xC000),
            _ => invalid_address!(addr),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.chr
                    .write_u8(BankSelect::Index(self.chr_select.get()), addr, value)
            }
            0x6000..=0x7FFF => panic!("cartridge has no prg_ram to write at 0x{:X?}", addr),
            0x8000..=0xFFFF => panic!("tried writing to a read only location 0x{:X?}", addr),
            _ => invalid_address!(addr),
        }
    }
}
