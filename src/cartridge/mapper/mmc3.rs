/// https://www.nesdev.org/wiki/MMC3
///
use crate::cartridge::mapper::{BankSelect, Switched};
use crate::cartridge::rom::RomData;
use crate::cartridge::NametableMirroring;
use crate::memory::{AddressSpace, Dyn};
use bitregions::bitregions;
use std::cell::Cell;

bitregions! {
    Select u8 {
        // Specify which bank register to update on next write to Bank Data register
        //   000: R0: Select 2 KB CHR bank at PPU $0000-$07FF (or $1000-$17FF)
        //   001: R1: Select 2 KB CHR bank at PPU $0800-$0FFF (or $1800-$1FFF)
        //   010: R2: Select 1 KB CHR bank at PPU $1000-$13FF (or $0000-$03FF)
        //   011: R3: Select 1 KB CHR bank at PPU $1400-$17FF (or $0400-$07FF)
        //   100: R4: Select 1 KB CHR bank at PPU $1800-$1BFF (or $0800-$0BFF)
        //   101: R5: Select 1 KB CHR bank at PPU $1C00-$1FFF (or $0C00-$0FFF)
        //   110: R6: Select 8 KB PRG ROM bank at $8000-$9FFF (or $C000-$DFFF)
        //   111: R7: Select 8 KB PRG ROM bank at $A000-$BFFF
        REGISTER: 0b0000_0111,
        // PRG ROM bank mode (0: $8000-$9FFF swappable,
        //                       $C000-$DFFF fixed to second-last bank;
        //                    1: $C000-$DFFF swappable,
        //                       $8000-$9FFF fixed to second-last bank)
        PRG_MODE: 0b0100_0000,
        // CHR A12 inversion (0: two 2 KB banks at $0000-$0FFF,
        //                       four 1 KB banks at $1000-$1FFF;
        //                    1: two 2 KB banks at $1000-$1FFF,
        //                       four 1 KB banks at $0000-$0FFF)
        CHR_MODE: 0b1000_0000,
    }
}

pub(super) struct MMC3 {
    mirroring: NametableMirroring,
    prg_rom: Switched<Dyn>,
    registers: Cell<[u8; 8]>,
    bank_select: Cell<Select>,
}

impl MMC3 {
    fn read_register(&self, index: usize) -> u8 {
        let cells = (&self.registers as &Cell<[u8]>).as_slice_of_cells();
        cells[index].get()
    }
    fn write_register(&self, value: u8) {
        let cells = (&self.registers as &Cell<[u8]>).as_slice_of_cells();
        cells[self.bank_select.get().register() as usize].set(value);
    }
}

impl super::Mapper for MMC3 {
    fn name(&self) -> &'static str {
        "MMC3"
    }

    fn nametable_mirroring(&self) -> NametableMirroring {
        self.mirroring
    }
}

impl From<&super::Rom> for MMC3 {
    fn from(rom: &super::Rom) -> Self {
        let rom_data = RomData::new(rom);
        MMC3 {
            mirroring: rom.nametable_mirroring,
            prg_rom: Switched::new(rom_data.prg_rom.clone(), rom_data.prg_rom.size(), 8_192),
            registers: Cell::default(),
            bank_select: Cell::default(),
        }
    }
}

impl AddressSpace for MMC3 {
    fn read_u8(&self, addr: u16) -> u8 {
        match (addr, self.bank_select.get().prg_mode()) {
            (0x8000..=0x9FFF, false) => self
                .prg_rom
                .read_u8(BankSelect::Index(self.read_register(6)), addr - 0x8000),
            (0x8000..=0x9FFF, true) => self.prg_rom.read_u8(BankSelect::Back(1), addr - 0x8000),

            _ => invalid_address!(addr),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match (addr, addr % 2) {
            // Even Bank Data (0x8000, 0x8002, etc.)
            (0x8000..=0x9FFF, 0) => self.bank_select.set(Select::from(value)),
            // Odd Bank Data (0x8001, 0x8003, etc.)
            (0x8000..=0x9FFF, 1) => self.write_register(value),

            _ => invalid_address!(addr),
        }
    }
}
