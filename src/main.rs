#![feature(coroutines, coroutine_trait)]

extern crate yaner;

use clap::Parser;

use std::convert::TryFrom;
use std::num::ParseIntError;
use std::path::PathBuf;

use yaner::cartridge::Cartridge;
use yaner::nes::Nes;

fn parse_hex(input: &str) -> Result<u16, ParseIntError> {
    u16::from_str_radix(input, 16)
}

#[derive(Debug, Parser)]
#[command(about = "Yet Another NES Emulator in Rust")]
struct Yaner {
    // #[arg(short = 'v', long = "verbose", action=clap::ArgAction::Count)]
    // verbose: u8,
    #[command(flatten)]
    verbose: clap_verbosity::Verbosity,
    #[command(subcommand)]
    cmd: YanerCommand,
}

#[derive(Debug, Parser)]
enum YanerCommand {
    Info {
        rom: PathBuf,
    },
    Run {
        #[arg(short, value_parser = parse_hex)]
        /// Sets the initial PC value instead of reading it from the reset vector.
        pc: Option<u16>,
        /// The iNES ROM file.
        rom: PathBuf,
    },
    Debug {
        #[arg(short, value_parser = parse_hex)]
        /// Sets the initial PC value instead of reading it from the reset vector.
        pc: Option<u16>,
        /// The iNES ROM file.
        rom: PathBuf,
    },
}

fn main() {
    let options = Yaner::parse();

    stderrlog::new()
        .module(module_path!())
        .verbosity(options.verbose.log_level_filter())
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
    }
}
