use super::rom::Rom;
use super::Mapper;
use crate::memory::AddressSpace;

pub mod nrom;
pub mod sxrom;

pub struct Unknown();

impl Mapper for Unknown {
    fn name(&self) -> String {
        "Unknown".to_string()
    }

    fn cpu_addr_space(&self) -> &dyn AddressSpace {
        unimplemented!()
    }

    fn ppu_addr_space(&self) -> &dyn AddressSpace {
        unimplemented!()
    }
}

pub fn from(rom: &Rom) -> Box<dyn Mapper> {
    match rom.mapper {
        0 => Box::new(nrom::NROM::from(rom)),
        1 => Box::new(sxrom::SxROM::from(rom)),
        _ => Box::new(Unknown()), // NOTE: this allows parsing to succeed, but running it will fail
    }
}

enum BankSelect {
    First,
    Last,
    Index(u8),
}

struct Switched<T: AddressSpace> {
    addr_space: T,
    addr_space_size: usize,
    bank_size: u16,
}

impl<T: AddressSpace> Switched<T> {
    fn new(addr_space: T, addr_space_size: usize, bank_size: u16) -> Self {
        assert_eq!(
            addr_space_size % bank_size as usize,
            0,
            "Address space size ({}) must be a multiple of bank size ({})",
            addr_space_size,
            bank_size
        );
        Switched {
            addr_space,
            addr_space_size,
            bank_size,
        }
    }

    fn addr_offset(&self, select: BankSelect) -> u16 {
        match select {
            BankSelect::First => 0,
            BankSelect::Last => (self.addr_space_size - self.bank_size as usize) as u16,
            BankSelect::Index(idx) => idx as u16 * self.bank_size,
        }
    }

    fn read_u8(&self, select: BankSelect, addr: u16) -> u8 {
        let addr_offset = self.addr_offset(select);
        self.addr_space.read_u8(addr_offset + addr)
    }

    fn write_u8(&self, select: BankSelect, addr: u16, value: u8) {
        let addr_offset = self.addr_offset(select);
        self.addr_space.write_u8(addr_offset + addr, value);
    }
}
