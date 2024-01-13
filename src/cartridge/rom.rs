use crate::memory::{AddressSpace, Dyn};
use bitflags::bitflags;
use nom::{error::ErrorKind, number::streaming::be_u8};
use std::convert::TryFrom;

use super::NametableMirroring;

#[derive(Debug)]
pub struct Header {
    prg_rom_size: u8,
    chr_rom_size: u8,
    flags_6: Flags6,
    #[allow(dead_code)]
    flags_7: Flags7,
    mapper: u8,
    prg_ram_size: u8,
    flags_9: Flags9,
    #[allow(dead_code)]
    flags_10: u8,
}

bitflags! {
    // https://www.nesdev.org/wiki/INES#Flags_6
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct Flags6: u8 {
        const VERTICAL = 0b0000_0001; // 0 == horizontal, 1 == vertical
        const PRG_RAM = 0b0000_0010;
        const TRAINER = 0b0000_0100;
        const FOUR_SCREEN = 0b0000_1000;
        const MAPPER_LO = 0b1111_0000;
    }
}

bitflags! {
    // https://www.nesdev.org/wiki/INES#Flags_7
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct Flags7: u8 {
        const VS_UNISYSTEM = 0b0000_0001;
        const PLAYCHOICE_10 = 0b0000_0010;
        const NES2_0 = 0b0000_0100; // NES2 format: this bit is not set
        const NES2_1 = 0b0000_1000; // NES2 format: this bit is set
        const MAPPER_HI = 0b1111_0000;
    }
}

impl Flags7 {
    fn is_nes2(&self) -> bool {
        self.contains(Flags7::NES2_1) && !self.contains(Flags7::NES2_0)
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct Flags9: u8 {
        const PAL = 0b0000_0001;
    }
}

impl From<Flags6> for NametableMirroring {
    fn from(flags: Flags6) -> Self {
        if flags.contains(Flags6::FOUR_SCREEN) {
            NametableMirroring::FourScreen
        } else if flags.contains(Flags6::VERTICAL) {
            NametableMirroring::Vertical
        } else {
            NametableMirroring::Horizontal
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum TvStandard {
    NTSC,
    PAL,
}

impl From<Flags9> for TvStandard {
    fn from(flags: Flags9) -> Self {
        if flags.contains(Flags9::PAL) {
            TvStandard::PAL
        } else {
            TvStandard::NTSC
        }
    }
}

#[derive(Clone)]
pub enum Chr {
    Rom(Vec<u8>),
    Ram(Dyn),
}

impl Chr {
    pub fn addr_space_size(&self) -> usize {
        match self {
            Chr::Ram(_) => 8_192,
            Chr::Rom(rom) => rom.len(),
        }
    }
}

impl AddressSpace for Chr {
    fn read_u8(&self, addr: u16) -> u8 {
        match self {
            Chr::Rom(rom) => rom[addr as usize % rom.len()],
            Chr::Ram(ram) => ram.read_u8(addr % 8_192),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match self {
            Chr::Rom(_) => (),
            Chr::Ram(ram) => ram.write_u8(addr % 8_192, value),
        }
    }
}

pub struct RomData {
    pub prg_rom: Dyn,
    pub prg_ram: Dyn,
    pub chr: Chr,
}

impl RomData {
    pub fn new(rom: &Rom) -> Self {
        let chr = match rom.chr_rom.len() {
            0 => Chr::Ram(Dyn::with_capacity(8_192)),
            _ => Chr::Rom(rom.chr_rom.clone()),
        };
        RomData {
            prg_rom: Dyn::new(rom.prg_rom.clone()),
            prg_ram: Dyn::with_capacity(rom.prg_ram_size),
            chr,
        }
    }
}

#[derive(Debug)]
pub struct Rom {
    pub title: Option<String>,
    pub mapper: u8,
    pub nametable_mirroring: NametableMirroring,
    pub tv_standard: TvStandard,
    pub prg_ram_size: usize,
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum RomError {
    InvalidFormat,
    ParseError(String),
    UnexpectedEof,
}

fn flags<F, T>(flags: F, i: &[u8]) -> nom::IResult<&[u8], T>
where
    F: Fn(u8) -> Option<T>,
{
    nom::combinator::map_opt(be_u8, flags)(i)
}

fn ines_header(i: &[u8]) -> nom::IResult<&[u8], Header> {
    let (i, _) = nom::bytes::streaming::tag(b"NES\x1A")(i)?;
    let (i, prg_rom_size) = be_u8(i)?;
    let (i, chr_rom_size) = be_u8(i)?;
    let (i, flags_6) = flags(Flags6::from_bits, i)?;
    let (i, flags_7) = flags(Flags7::from_bits, i)?;
    let (i, prg_ram_size) = be_u8(i)?;
    let (i, flags_9) = flags(Flags9::from_bits, i)?;
    let (i, flags_10) = be_u8(i)?;
    let (i, _) = nom::bytes::streaming::take(5_usize)(i)?;

    let mapper = (flags_7.bits() & 0xF0) | (flags_6.bits() >> 4);

    let header = Header {
        prg_rom_size,
        chr_rom_size,
        flags_6,
        flags_7,
        mapper,
        prg_ram_size,
        flags_9,
        flags_10,
    };
    Ok((i, header))
}

fn ines_rom(i: &[u8]) -> nom::IResult<&[u8], Rom> {
    let (i, header) = nom::combinator::verify(ines_header, |h| !h.flags_7.is_nes2())(i)?;

    // Trainer is not present on cartridge dumps; ignore it if present.
    // https://www.nesdev.org/wiki/INES#Trainer
    let (i, _) = nom::combinator::cond(
        header.flags_6.contains(Flags6::TRAINER),
        nom::bytes::streaming::take(512_usize),
    )(i)?;

    let (i, prg_rom) = nom::bytes::streaming::take(header.prg_rom_size as usize * 16_384)(i)?;
    let (title_bytes, chr_rom) =
        nom::bytes::streaming::take(header.chr_rom_size as usize * 8_192)(i)?;
    let rom = Rom {
        title: Some(String::from_utf8_lossy(title_bytes).into_owned()).filter(|s| !s.is_empty()),
        mapper: header.mapper,
        nametable_mirroring: header.flags_6.into(),
        tv_standard: header.flags_9.into(),
        prg_ram_size: if header.prg_ram_size == 0 {
            8_192
        } else {
            header.prg_ram_size as usize * 8_192
        },
        prg_rom: prg_rom.into(),
        chr_rom: chr_rom.into(),
    };
    Ok((&[], rom))
}

impl TryFrom<&[u8]> for Rom {
    type Error = RomError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        match ines_rom(value) {
            Ok((_, rom)) => Ok(rom),
            Err(nom::Err::Incomplete(_)) => Err(RomError::UnexpectedEof),
            Err(nom::Err::Error(error)) => {
                if matches!(error.code, ErrorKind::Tag) {
                    Err(RomError::InvalidFormat) // tag error means the magic header didn't match
                } else {
                    Err(RomError::ParseError(error.code.description().into()))
                }
            }
            Err(nom::Err::Failure(error)) => {
                Err(RomError::ParseError(error.code.description().into()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_success() -> Result<(), RomError> {
        let nestest_bytes: &[u8] = include_bytes!("../../roms/nes-test-roms/other/nestest.nes");
        let nestest_rom = Rom::try_from(nestest_bytes)?;

        assert_eq!(None, nestest_rom.title);
        assert_eq!(0, nestest_rom.mapper);
        assert_eq!(
            NametableMirroring::Horizontal,
            nestest_rom.nametable_mirroring
        );
        assert_eq!(TvStandard::NTSC, nestest_rom.tv_standard);
        assert_eq!(8192, nestest_rom.prg_ram_size);
        assert_eq!(16_384, nestest_rom.prg_rom.len());
        assert_eq!(8_192, nestest_rom.chr_rom.len());
        Ok(())
    }

    #[test]
    fn test_invalid_magic_header() {
        let nestest_bytes: &[u8] = include_bytes!("../../roms/nes-test-roms/other/nestest.nes");
        match Rom::try_from(&nestest_bytes[1..]) {
            Ok(_) => panic!("unexpected success"),
            Err(error) => assert_eq!(RomError::InvalidFormat, error),
        }
    }

    #[test]
    fn test_invalid_truncated() {
        let nestest_bytes: &[u8] = include_bytes!("../../roms/nes-test-roms/other/nestest.nes");
        match Rom::try_from(&nestest_bytes[..100]) {
            Ok(_) => panic!("unexpected success"),
            Err(error) => assert_eq!(RomError::UnexpectedEof, error),
        }
    }
}
