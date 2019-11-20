use std::convert::TryFrom;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use structopt::StructOpt;

mod memory;
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
            let mut f = File::open(rom).unwrap();
            let mut buffer = Vec::new();
            // read the whole file
            f.read_to_end(&mut buffer).unwrap();
            println!("{:?}", rom::Rom::try_from(buffer.as_slice()).unwrap());
        }
    }
}
