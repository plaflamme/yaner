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

#[macro_use] mod helper;
mod memory;
mod cartridge;
mod cpu;
mod ppu;
mod nes;

#[derive(Debug, StructOpt)]
#[structopt(about = "Yet Another NES Emulator in Rust")]
enum Yaner {
    Info {
        rom: PathBuf
    },
    Run {
        rom: PathBuf
    }
}

fn main() {
    let options = Yaner::from_args();
    use Yaner::*;
    match options {
        Info { rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            println!("{}", cartridge);
        },
        Run { rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            let nes = crate::nes::Nes::new(cartridge);
            nes.run();
        },
    }
}
