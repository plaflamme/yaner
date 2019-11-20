use std::convert::TryFrom;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use structopt::StructOpt;
use crate::cartridge::Cartridge;

mod memory;
mod cartridge;
mod cpu;
mod nes;
mod rom;

#[derive(Debug, StructOpt)]
#[structopt(about = "Yet Another NES Emulator in Rust")]
enum Yaner {
    Generate,
    Info {
        rom: PathBuf
    }
}

fn main() {
    let options = Yaner::from_args();
    use Yaner::*;
    match options {
        Generate => {
            cpu::generator::generate_opcode_table()
        },
        Info { rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            println!("{}", cartridge);
        }
    }
}
