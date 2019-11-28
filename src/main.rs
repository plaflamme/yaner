#![feature(
    generators, generator_trait
)]

use std::convert::TryFrom;
use std::path::PathBuf;
use structopt::StructOpt;
use crate::cartridge::Cartridge;
use std::num::ParseIntError;

#[macro_use]
extern crate log;

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
struct Yaner {
    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    verbose: usize,
    #[structopt(subcommand)]
    cmd: YanerCommand
}

#[derive(Debug, StructOpt)]
enum YanerCommand {
    Info {
        rom: PathBuf
    },
    Run {
        #[structopt(short, parse(try_from_str = parse_hex))]
        pc: Option<u16>,
        rom: PathBuf,
    },
    Generate,
}

fn main() {
    let options = Yaner::from_args();

    stderrlog::new()
        .module(module_path!())
        .verbosity(options.verbose)
        .timestamp(stderrlog::Timestamp::Off)
        .init()
        .unwrap();

    use YanerCommand::*;
    match options.cmd {
        Info { rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            println!("{}", cartridge);
        },
        Run { pc, rom  } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            let nes = crate::nes::Nes::new(cartridge);
            nes.run(pc);
        },
        Generate => {
            crate::cpu::generator::generate_opcode_table()
        },
    }
}
