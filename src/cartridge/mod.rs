use crate::rom::{Rom, RomError};
use std::convert::TryFrom;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use std::fmt::{Display, Formatter, Error};

pub struct Cartridge {
    // TODO: mapper: Mapper,
    path: PathBuf,
    rom: Rom
    // TODO: memory backed RAM
}

impl Display for Cartridge {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        writeln!(f, "{} - {}", self.path.display(), self.rom.title.as_ref().unwrap_or(&"<no title>".to_string()))?;
        writeln!(f, "\tPRG ROM size: {} bytes", self.rom.prg_rom.len())?;
        writeln!(f, "\tCHR ROM size: {} bytes", self.rom.chr_rom.len())?;
        writeln!(f, "\tPRG RAM size: {} bytes", self.rom.prg_ram_size)?;
        writeln!(f, "\tMirroring   : {:?}", self.rom.nametable_mirroring)?;
        writeln!(f, "\tTV Standard : {:?}", self.rom.tv_standard)
    }
}

#[derive(Debug)]
pub enum CartridgeError {
    RomError(RomError),
    IoError(std::io::Error)
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
        f.read_to_end(&mut buf);
        let rom = Rom::try_from(buf.as_slice())?;

        Ok(
            Cartridge {
                path: path,
                rom
            }
        )
    }
}
