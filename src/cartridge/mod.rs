use crate::memory::AddressSpace;
use std::convert::TryFrom;
use std::fmt::{Display, Error, Formatter};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

mod mapper;
mod rom;

use rom::{Rom, RomError};

// https://wiki.nesdev.com/w/index.php/Mirroring#Nametable_Mirroring
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NametableMirroring {
    Horizontal, // vertical arrangement
    Vertical,   // horizontal arrangement
    FourScreen,
}

pub trait Mapper: AddressSpace {
    fn name(&self) -> &'static str;

    fn nametable_mirroring(&self) -> NametableMirroring;
}

pub struct Cartridge {
    path: PathBuf,
    rom: Rom,
    // TODO: memory backed RAM
}

impl Cartridge {
    pub fn mapper(&self) -> Box<dyn Mapper> {
        mapper::from(&self.rom)
    }
}

impl Display for Cartridge {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        writeln!(
            f,
            "{} - {}",
            self.path.display(),
            self.rom.title.as_ref().unwrap_or(&"<no title>".to_string())
        )?;
        writeln!(f, "\tPRG ROM size: {} bytes", self.rom.prg_rom.len())?;
        writeln!(f, "\tCHR ROM size: {} bytes", self.rom.chr_rom.len())?;
        writeln!(f, "\tPRG RAM size: {} bytes", self.rom.prg_ram_size)?;
        writeln!(
            f,
            "\tMapper      : {} ({})",
            self.mapper().name(),
            self.rom.mapper
        )?;
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
        Cartridge::try_from(buf.as_slice())
    }
}

impl TryFrom<&[u8]> for Cartridge {
    type Error = CartridgeError;

    fn try_from(data: &[u8]) -> Result<Self, Self::Error> {
        let rom = Rom::try_from(data)?;

        Ok(Cartridge {
            path: PathBuf::from(""),
            rom,
        })
    }
}
