use bitflags::bitflags;
use crate::cartridge::rom::Rom;
use crate::memory::{Ram8KB, AddressSpace};
use super::Mapper;
use std::cell::Cell;

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

bitflags!(
    struct Ctrl: u8 {
        const Mirroring = 0b0000_0011;
        const PRG_BANK = 0b0000_1100;
        const CHR_BANK = 0b0001_0000;
    }
);

// http://wiki.nesdev.com/w/index.php/MMC1
pub struct SxROM {
    prg_ram: Ram8KB,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
    chr_ram: Ram8KB,
    shift_register: Cell<ShiftRegister>,
    control: Cell<Ctrl>,
    prg_bank: Cell<u8>,
    chr_bank_0: Cell<u8>,
    chr_bank_1: Cell<u8>
}

impl From<&Rom> for SxROM {
    fn from(rom: &Rom) -> Self {
        SxROM {
            prg_ram: Ram8KB::new(),
            prg_rom: rom.prg_rom.clone(),
            chr_rom: rom.chr_rom.clone(),
            chr_ram: Ram8KB::new(),
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

    fn as_addr_space(&self) -> &dyn AddressSpace {
        self
    }
}

impl AddressSpace for SxROM {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x0FFF => unimplemented!(), // TODO: 4 KB switchable CHR bank
            0x1000..=0x1FFF => unimplemented!(), // TODO: 4 KB switchable CHR bank
            0x6000..=0x7FFF => self.prg_ram.read_u8(addr - 0x6000),
            0x8000..=0xBFFF => self.prg_rom[(addr - 0x8000) as usize % self.prg_rom.len()], // TODO: switching
            0xC000..=0xFFFF => self.prg_rom[(addr - 0xC000) as usize % self.prg_rom.len()], // TODO: switching
            _ => invalid_address!(addr)
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x0FFF => self.chr_ram.write_u8(addr, value),
            0x1000..=0x1FFF => self.chr_ram.write_u8(addr - 0x1000, value),
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
