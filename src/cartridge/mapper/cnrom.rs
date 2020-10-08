use crate::cartridge::{NametableMirroring, Mapper};
use crate::cartridge::rom::{RomData, Rom, Chr};
use crate::memory::AddressSpace;
use std::cell::Cell;
use crate::cartridge::mapper::{Switched, BankSelect};

pub struct CNROM {
    mirroring: NametableMirroring,
    prg_rom: Switched<crate::memory::Rom>,
    chr: Switched<Chr>,
    chr_select: Cell<u8>,
}

impl From<&Rom> for CNROM {
    fn from(rom: &Rom) -> Self {
        let data = RomData::new(&rom);
        CNROM {
            mirroring: rom.nametable_mirroring,
            prg_rom: Switched::new(crate::memory::Rom::new(data.prg_rom.clone()), data.prg_rom.len(), 16_384),
            chr: Switched::new(data.chr.clone(), data.chr.addr_space_size(), 8_192),
            chr_select: Cell::default(),
        }
    }
}

impl Mapper for CNROM {
    fn name(&self) -> &'static str { "CNROM" }

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
                self.chr.read_u8(BankSelect::Index(self.chr_select.get()), addr)
            },

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
            0x0000..=0x1FFF => self.chr.write_u8(BankSelect::Index(self.chr_select.get()), addr, value),
            0x6000..=0x7FFF => panic!("cartridge has no prg_ram to write at 0x{:X?}", addr),
            0x8000..=0xFFFF => panic!("tried writing to a read only location 0x{:X?}", addr),
            _ => invalid_address!(addr),
        }
    }
}