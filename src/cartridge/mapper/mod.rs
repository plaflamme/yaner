use crate::memory::AddressSpace;
use super::rom::Rom;
use super::Mapper;

pub mod nrom;
pub mod sxrom;

pub struct Unknown();

impl Mapper for Unknown {
    fn name(&self) -> String {
        "Unknown".to_string()
    }

    fn as_addr_space(&self) -> &dyn AddressSpace {
        self
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

pub fn from(rom: &Rom) -> Box<dyn Mapper> {
    match rom.mapper {
        0 => Box::new(nrom::NROM::from(rom)),
        1 => Box::new(sxrom::SxROM::from(rom)),
        _ => Box::new(Unknown()), // NOTE: this allows parsing to succeed, but running it will fail
    }
}
