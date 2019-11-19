use clap::{Arg, App, SubCommand};
use std::convert::TryFrom;
use std::fs::File;
use std::io::Read;

mod memory;
mod cpu;
mod nes;
mod rom;

fn main() {

    let matches = App::new("rnes")
        .subcommand(
            SubCommand::with_name("generate").arg(
                Arg::with_name("opcode").short("o").long("opcodes").help("Generate OpCode table")
            )
        )
        .subcommand(
            SubCommand::with_name("info").arg(
                Arg::with_name("rom").short("r").long("rom").required(true).takes_value(true).help("ROM file")
            )
        )
        .get_matches();

    match matches.subcommand() {
        ("generate", Some(args)) => {
            if args.is_present("opcode") {
                cpu::generator::generate_opcode_table()
            }
        },
        ("info", Some(args)) => {
            match args.value_of("rom") {
                Some(filename) => {
                    let mut f = File::open(filename).unwrap();
                    let mut buffer = Vec::new();
                    // read the whole file
                    f.read_to_end(&mut buffer).unwrap();
                    println!("{:?}", rom::Rom::try_from(buffer.as_slice()).unwrap());
                },
                None => unimplemented!()
            }
        },
        _ => unimplemented!()
    }
}
