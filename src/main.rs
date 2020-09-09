#![feature(generators, generator_trait)]

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
        #[structopt(short, parse(try_from_str = parse_hex))]
        /// Outputs a u16 value at this memory location after execution.
        output: Option<u16>,
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
        Run { pc, output, rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            let nes = Nes::new(cartridge);
            nes.run(pc);
            if let Some(addr) = output {
                let value = nes.debug().cpu_bus.read_u16(addr);
                println!("{:#04X} -> {:#04X}", addr, value);
            }
        }
        Debug { pc, rom } => {
            let cartridge = Cartridge::try_from(rom).unwrap();
            let nes = Nes::new(cartridge);
            let debugger = yaner::tui::Debugger::new(nes);
            debugger.start(pc).unwrap();
        }
        Generate => yaner::cpu::generator::generate_opcode_table(),
    }
}
