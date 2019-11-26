#![feature(
    never_type,
    generators, generator_trait
)]

use std::convert::TryFrom;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use structopt::StructOpt;
use crate::cartridge::Cartridge;
use std::num::ParseIntError;

#[macro_use] mod helper;
mod memory;
mod cartridge;
mod cpu;
mod ppu;
mod nes;

fn parse_hex(input: &str) -> Result<u16, ParseIntError> {
    u16::from_str_radix(input, 16)
}

#[derive(Debug, StructOpt)]
#[structopt(about = "Yet Another NES Emulator in Rust")]
enum Yaner {
    Info {
        rom: PathBuf
    },
    Run {
        #[structopt(short, parse(try_from_str = parse_hex))]
        pc: Option<u16>,
        rom: PathBuf
    },
    Generate,
}

fn main() {
    let options = Yaner::from_args();
    use Yaner::*;
    match options {
        Info { rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            println!("{}", cartridge);
        },
        Run { pc, rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            let nes = crate::nes::Nes::new(cartridge);
            nes.run(pc);
        },
        Generate => {
            crate::cpu::generator::generate_opcode_table()
        },
    }
}
