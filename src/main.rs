#![feature(coroutines, coroutine_trait)]

extern crate yaner;

use std::convert::TryFrom;
use std::num::ParseIntError;
use std::path::PathBuf;

use structopt::StructOpt;

use yaner::cartridge::Cartridge;
use yaner::nes::Nes;

fn parse_hex(input: &str) -> Result<u16, ParseIntError> {
    u16::from_str_radix(input, 16)
}

#[derive(Debug, StructOpt)]
#[structopt(about = "Yet Another NES Emulator in Rust")]
struct Yaner {
    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    verbose: usize,
    #[structopt(subcommand)]
    cmd: YanerCommand,
}

#[derive(Debug, StructOpt)]
enum YanerCommand {
    Info {
        rom: PathBuf,
    },
    Run {
        #[structopt(short, parse(try_from_str = parse_hex))]
        /// Sets the initial PC value instead of reading it from the reset vector.
        pc: Option<u16>,
        /// The iNES ROM file.
        rom: PathBuf,
    },
    Debug {
        #[structopt(short, parse(try_from_str = parse_hex))]
        /// Sets the initial PC value instead of reading it from the reset vector.
        pc: Option<u16>,
        /// The iNES ROM file.
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
        }
        Run { pc, rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            let nes = Nes::new_with_pc(cartridge, pc);
            nes.steps().run().unwrap();
        }
        Debug { pc, rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            let nes = Nes::new_with_pc(cartridge, pc);
            let mut debugger = yaner::tui::Debugger::new(nes);
            debugger.start().unwrap();
        }
        Generate => yaner::cpu::generator::generate_opcode_table(),
    }
}
