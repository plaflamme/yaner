use super::rom::Rom;
use super::Mapper;
use super::NametableMirroring;
use crate::memory::AddressSpace;

pub mod cnrom;
pub mod nrom;
pub mod sxrom;

pub struct Unknown();

impl Mapper for Unknown {
    fn name(&self) -> &'static str {
        "Unknown"
    }

    fn nametable_mirroring(&self) -> NametableMirroring {
        unimplemented!()
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
        3 => Box::new(cnrom::CNROM::from(rom)),
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
    bank_size: usize,
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
            bank_size: bank_size as usize,
        }
    }

    fn addr_offset(&self, select: BankSelect) -> u16 {
        match select {
            BankSelect::First => 0,
            BankSelect::Last => (self.addr_space_size - self.bank_size) as u16,
            BankSelect::Index(idx) => (idx as usize * self.bank_size) as u16,
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::memory::Ram8KB;

    #[test]
    #[should_panic]
    fn test_switched_requires_alignment() {
        Switched::new(Ram8KB::new(), 8_192, 100);
    }

    #[test]
    fn test_switched_select_first() {
        let ram = Ram8KB::new();
        for i in 0..1024u16 {
            ram.write_u8(i as u16, (i % 256) as u8);
        }
        let switched = Switched::new(ram, 8_192, 1_024);
        for i in 0..1024 {
            assert_eq!((i % 256) as u8, switched.read_u8(BankSelect::First, i as u16));
            assert_eq!(0, switched.read_u8(BankSelect::Last, i as u16));
        }
    }

    #[test]
    fn test_switched_select_last() {
        let ram = Ram8KB::new();
        for i in (8_192 - 1_024)..8_192u16 {
            ram.write_u8(i as u16, (i % 256) as u8);
        }
        let switched = Switched::new(ram, 8_192, 1_024);
        for i in 0..1024 {
            assert_eq!((i % 256) as u8, switched.read_u8(BankSelect::Last, i as u16));
            assert_eq!(0, switched.read_u8(BankSelect::First, i as u16));
        }
    }

    #[test]
    fn test_switched_select_index() {
        let ram = Ram8KB::new();
        for page in 0..8u16 {
            for i in 0..1024 {
                ram.write_u8(page * 1024 + i, page as u8);
            }
        }
        let switched = Switched::new(ram, 8_192, 1_024);
        for page in 0..8u8 {
            for i in 0..1024 {
                assert_eq!(page, switched.read_u8(BankSelect::Index(page), i as u16));
            }
        }
    }
}
