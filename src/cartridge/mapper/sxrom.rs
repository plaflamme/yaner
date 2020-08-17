use bitflags::bitflags;
use crate::cartridge::rom::{Rom, RomData, Chr};
use crate::memory::{AddressSpace, Ram};
use super::{Mapper, Switched};
use std::cell::Cell;
use crate::cartridge::mapper::BankSelect;

#[derive(Clone, Copy)]
struct ShiftRegister {
    value: u8,
    next_bit: u8
}

impl ShiftRegister {
    fn new() -> Self {
        ShiftRegister { value: 0, next_bit: 0 }
    }

    fn reset(&mut self) {
        self.value = 0;
        self.next_bit = 0;
    }

    fn push(&mut self, v: u8) -> Option<u8> {
        if v & 0x80 != 0 {
            self.reset();
            None
        } else {
            let bit = v << self.next_bit;
            self.value |= bit;
            self.next_bit += 1;
            if self.next_bit != 5 { None } else {
                let result = self.value;
                self.reset();
                Some(result)
            }
        }
    }
}

enum PrgBankMode {
    Consecutive,
    FixFirstLow,
    FixLastHigh,
}

enum ChrBankMode {
    Consecutive,
    Disjoint,
}

bitflags!(
    struct Ctrl: u8 {
        const Mirroring = 0b0000_0011;
        const PRG_BANK_0 = 0b0000_0100;
        const PRG_BANK_1 = 0b0000_1000;
        const CHR_BANK_MODE = 0b0001_0000; // 0: switch 8KB at a time; 1: switch 4KB separately
    }
);

impl Ctrl {
    fn chr_bank_mode(&self) -> ChrBankMode {
        if self.contains(Ctrl::CHR_BANK_MODE) {
            ChrBankMode::Disjoint
        } else {
            ChrBankMode::Consecutive
        }
    }

    fn prg_bank_mode(&self) -> PrgBankMode {
        if self.contains(Self::PRG_BANK_0 | Self::PRG_BANK_1) {
            PrgBankMode::FixLastHigh
        } else if self.contains(Self::PRG_BANK_1) {
            PrgBankMode::FixFirstLow
        } else {
            PrgBankMode::Consecutive
        }
    }
}

enum BankAddr {
    Low,
    High
}

// http://wiki.nesdev.com/w/index.php/MMC1
pub struct SxROM {
    prg_rom: Switched<crate::memory::Rom>,
    chr: Switched<Chr>,
    prg_ram: Ram,
    shift_register: Cell<ShiftRegister>,
    control: Cell<Ctrl>,
    prg_bank: Cell<u8>,
    chr_bank_0: Cell<u8>,
    chr_bank_1: Cell<u8>
}

impl SxROM {
    fn chr_bank_select(&self, bank: BankAddr) -> BankSelect {
        match self.control.get().chr_bank_mode() {
            ChrBankMode::Disjoint => {
                match bank {
                    BankAddr::Low => BankSelect::Index(self.chr_bank_0.get()),
                    BankAddr::High => BankSelect::Index(self.chr_bank_1.get()),
                }
            },
            ChrBankMode::Consecutive => {
                match bank {
                    BankAddr::Low => BankSelect::Index(self.chr_bank_0.get()),
                    BankAddr::High => BankSelect::Index(self.chr_bank_0.get() + 1),
                }
            }
        }
    }

    fn prg_bank_select(&self, bank: BankAddr) -> BankSelect {
        match self.control.get().prg_bank_mode() {
            PrgBankMode::FixFirstLow => {
                match bank {
                    BankAddr::Low => BankSelect::First,
                    BankAddr::High => BankSelect::Index(self.prg_bank.get()),
                }
            },
            PrgBankMode::FixLastHigh => {
                match bank {
                    BankAddr::Low => BankSelect::Index(self.prg_bank.get()),
                    BankAddr::High => BankSelect::Last,
                }
            },
            PrgBankMode::Consecutive => {
                match bank {
                    BankAddr::Low => BankSelect::Index(self.prg_bank.get() & 0xFE), // clear low bit
                    BankAddr::High => BankSelect::Index(self.prg_bank.get() | 1), // set low bit
                }
            }
        }
    }
}

impl From<&Rom> for SxROM {
    fn from(rom: &Rom) -> Self {
        let data = RomData::new(rom);

        let prg_rom = Switched::new(
            crate::memory::Rom::new(rom.prg_rom.clone()),
            rom.prg_rom.len(),
            16_384
        );

        let size = data.chr.addr_space_size();
        let chr = Switched::new(
            data.chr,
            size,
            4_096,
        );

        SxROM {
            prg_rom,
            chr,
            prg_ram: data.prg_ram,
            shift_register: Cell::new(ShiftRegister::new()),
            control: Cell::new(Ctrl::empty()),
            prg_bank: Cell::new(0),
            chr_bank_0: Cell::new(0),
            chr_bank_1: Cell::new(0),
        }
    }
}

impl Mapper for SxROM {
    fn name(&self) -> String {
        "SxROM".to_string()
    }


    fn cpu_addr_space(&self) -> &dyn AddressSpace {
        self // TODO
    }

    fn ppu_addr_space(&self) -> &dyn AddressSpace {
        self // TODO
    }
}

impl AddressSpace for SxROM {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x0FFF => self.chr.read_u8(self.chr_bank_select(BankAddr::Low), addr),
            0x1000..=0x1FFF => self.chr.read_u8(self.chr_bank_select(BankAddr::High), addr - 0x1000),
            0x6000..=0x7FFF => self.prg_ram.read_u8(addr - 0x6000),
            0x8000..=0xBFFF => self.prg_rom.read_u8(self.prg_bank_select(BankAddr::Low), addr - 0x8000),
            0xC000..=0xFFFF => self.prg_rom.read_u8(self.prg_bank_select(BankAddr::High), addr - 0xC000),
            _ => invalid_address!(addr)
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x0FFF => self.chr.write_u8(self.chr_bank_select(BankAddr::Low), addr, value),
            0x1000..=0x1FFF => self.chr.write_u8(self.chr_bank_select(BankAddr::High), addr - 0x1000, value),
            0x6000..=0x7FFF => self.prg_ram.write_u8(addr - 0x6000, value),
            0x8000..=0xFFFF => {
                let mut s = self.shift_register.get();
                if let Some(v) = s.push(value) {
                    match addr {
                        0x8000..=0x9FFF => self.control.set(Ctrl::from_bits_truncate(value)),
                        0xA000..=0xBFFF => self.chr_bank_0.set(v),
                        0xC000..=0xDFFF => self.chr_bank_1.set(v),
                        0xE000..=0xFFFF => self.prg_bank.set(v),
                        _ => invalid_address!(addr)
                    }
                }
            }
            _ => invalid_address!(addr)
        }
    }
}
