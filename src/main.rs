use clap::{Arg, App, SubCommand};

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
        .get_matches();

    match matches.subcommand() {
        ("generate", Some(args)) => {
            if args.is_present("opcode") {
                cpu::generator::generate_opcode_table()
            }
        },
        _ => unimplemented!()
    }
}
