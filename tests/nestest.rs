#![feature(generators, generator_trait)]

#[macro_use]
extern crate nom;

use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::ops::{Generator, GeneratorState};
use std::path::Path;
use std::pin::Pin;
use std::str::FromStr;

use nom::bytes::complete::take_while;
use nom::combinator::map_res;
use nom::{AsChar, IResult};

use yaner::cartridge::Cartridge;
use yaner::cpu::{CpuCycle, Flags};
use yaner::nes::{Nes, NesCycle};

#[derive(Debug)]
struct LogLine {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    flags: u8,
    sp: u8,
    ppu_frame: u32,
    ppu_dot: u32,
    cpu_cyc: u32,
}

impl Display for LogLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04X}", self.pc)
    }
}

named!(hex_u16<&str, u16>,
  map_res!(
    take_while!(|c:char| c.is_hex_digit()),
    |str| u16::from_str_radix(str, 16)
  )
);

named!(hex_u8<&str, u8>,
  map_res!(
    take_while!(|c:char| c.is_hex_digit()),
    |str| u8::from_str_radix(str, 16)
  )
);

// TODO: for some reason this doesn't produce the same result as the non-macro version
// named!(d_u32<&str, u32>,
//   map_res!(
//     take_while!(|c:char| c.is_digit(10)),
//     FromStr::from_str
//   )
// );

fn d_u32(input: &str) -> IResult<&str, u32> {
    map_res(take_while(|c: char| c.is_digit(10)), FromStr::from_str)(input)
}

named!(parse_logline<&str, LogLine>,
    do_parse!(
        pc: hex_u16 >>
        take!(12) >>
        op: take!(3) >>
        take!(29) >>
        tag!("A:") >> a: hex_u8 >> tag!(" ") >>
        tag!("X:") >> x: hex_u8 >> tag!(" ") >>
        tag!("Y:") >> y: hex_u8 >> tag!(" ") >>
        tag!("P:") >> flags: hex_u8 >> tag!(" ") >>
        tag!("SP:") >> sp: hex_u8 >> tag!(" ") >>
        tag!("PPU:") >> ppu_frame: ws!(d_u32) >> tag!(",") >> ppu_dot: ws!(d_u32) >>
        tag!("CYC:") >> cpu_cyc: ws!(d_u32) >>
        (
            LogLine {
                pc,
                a,
                x,
                y,
                flags,
                sp,
                ppu_dot,
                ppu_frame,
                cpu_cyc
            }
        )
    )
);

fn parse_log() -> Result<Vec<LogLine>, std::io::Error> {
    let log = std::fs::read_to_string("roms/nes-test-roms/other/nestest.log")?;
    let lines = log
        .lines()
        .map(|line| {
            let (_, log_line) =
                parse_logline(line).expect(format!("invalid log line {}", line).as_str());
            log_line
        })
        .collect::<Vec<LogLine>>();

    Ok(lines)
}

fn assert_log(nes: &Nes, line: &LogLine) {
    let state = nes.debug();
    assert_eq!(
        nes.clocks.cpu_cycles.get(),
        line.cpu_cyc as u64,
        "incorrect cpu cycle at {}",
        line
    );
    assert_eq!(state.cpu.pc, line.pc, "incorrect pc at {}", line);
    assert_eq!(state.cpu.a, line.a, "incorrect acc at {}", line);
    assert_eq!(state.cpu.x, line.x, "incorrect x at {}", line);
    assert_eq!(state.cpu.y, line.y, "incorrect y at {}", line);
    assert_eq!(state.cpu.flags, Flags::from_bits_truncate(line.flags), "incorrect flags at {}", line);
    assert_eq!(
        state.cpu.sp,
        line.sp,
        "incorrect stack pointer at {}",
        line
    );
    assert_eq!(
        nes.clocks.ppu_frame(),
        line.ppu_frame as u64,
        "incorrect ppu frame at {}",
        line
    );
    assert_eq!(
        nes.clocks.ppu_dot(),
        line.ppu_dot as u16,
        "incorrect ppu dot at {}",
        line
    );
    // TODO: check ppu scanline and cycle
}

// Steps the same way nintendulator does, which is:
//   (cpu_step + 3 * ppu_step)
//   if cpu_step == OpComplete { yield () }
fn nintendulator_steps(nes: &Nes) -> impl Generator<Yield = (), Return = ()> + '_ {
    let mut ppu_steps = nes.ppu_steps(Some(0xC000));
    move || {
        let mut yield_on_next = false;
        loop {
            match Generator::resume(Pin::new(&mut ppu_steps), ()) {
                GeneratorState::Yielded(NesCycle::PowerUp) => yield (),
                GeneratorState::Yielded(NesCycle::CpuCycle(CpuCycle::OpComplete(_, _), _)) => {
                    yield_on_next = true;
                }
                GeneratorState::Yielded(_) => (),
                GeneratorState::Complete(_) => break,
            }

            if yield_on_next && nes.clocks.ppu_cycles.get() % 3 == 0 {
                yield_on_next = false;
                yield ();
            }
        }
    }
}

#[test]
fn test_nestest() {
    let log = parse_log().expect("cannot parse log");

    let cartridge =
        Cartridge::try_from(Path::new("roms/nes-test-roms/other/nestest.nes").to_owned()).unwrap();
    let nes = Nes::new(cartridge);

    let mut steps = nintendulator_steps(&nes);

    let mut log_iter = log.iter();

    loop {
        match Generator::resume(Pin::new(&mut steps), ()) {
            GeneratorState::Yielded(_) => {
                match log_iter.next() {
                    Some(line) => assert_log(&nes, line),
                    None => (), // log is shorter than actual test
                }
            }
            GeneratorState::Complete(_) => break,
        };
    }

    let result = nes.debug().ram.read_u16(0x02);
    assert_eq!(0x00, result);
}
