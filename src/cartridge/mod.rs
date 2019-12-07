use std::convert::TryFrom;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use std::fmt::{Display, Formatter, Error};
use crate::memory::AddressSpace;

mod mapper;
mod rom;
mod sxrom;

use rom::{Rom, RomError};

pub trait Mapper: AddressSpace {
    fn name(&self) -> String;

    // upcast
    fn as_addr_space(&self) -> &dyn AddressSpace;
}

pub struct Cartridge {
    pub mapper: Box<dyn Mapper>,
    path: PathBuf,
    rom: Rom,
    // TODO: memory backed RAM
}

impl Display for Cartridge {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        writeln!(f, "{} - {}", self.path.display(), self.rom.title.as_ref().unwrap_or(&"<no title>".to_string()))?;
        writeln!(f, "\tPRG ROM size: {} bytes", self.rom.prg_rom.len())?;
        writeln!(f, "\tCHR ROM size: {} bytes", self.rom.chr_rom.len())?;
        writeln!(f, "\tPRG RAM size: {} bytes", self.rom.prg_ram_size)?;
        writeln!(f, "\tMapper      : {} ({})", self.mapper.name(), self.rom.mapper)?;
        writeln!(f, "\tMirroring   : {:?}", self.rom.nametable_mirroring)?;
        writeln!(f, "\tTV Standard : {:?}", self.rom.tv_standard)
    }
}

#[derive(Debug)]
pub enum CartridgeError {
    RomError(RomError),
    IoError(std::io::Error),
}

impl From<RomError> for CartridgeError {
    fn from(rom_error: RomError) -> Self {
        CartridgeError::RomError(rom_error)
    }
}

impl From<std::io::Error> for CartridgeError {
    fn from(io_error: std::io::Error) -> Self {
        CartridgeError::IoError(io_error)
    }
}

impl TryFrom<PathBuf> for Cartridge {
    type Error = CartridgeError;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        let mut f = File::open(path.clone())?;
        let mut buf = Vec::new();
        f.read_to_end(&mut buf)?;
        let rom = Rom::try_from(buf.as_slice())?;

        let mapper: Box<dyn Mapper> = match rom.mapper {
            0 => Box::new(mapper::NROM::from(&rom)),
            1 => Box::new(sxrom::SxROM::from(&rom)),
            _ => Box::new(mapper::Unknown()), // NOTE: this allows parsing to succeed, but running it will fail
        };

        Ok(
            Cartridge {
                mapper,
                path,
                rom
            }
        )
    }
}
