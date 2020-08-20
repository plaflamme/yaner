#![feature(generators, generator_trait)]

#[macro_use]
extern crate nom;

use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::ops::{Generator, GeneratorState};
use std::path::Path;
use std::pin::Pin;
use std::str::FromStr;

use nom::{AsChar, IResult};
use nom::bytes::complete::take_while;
use nom::combinator::map_res;

use yaner::cartridge::Cartridge;
use yaner::nes::Nes;

#[derive(Debug)]
struct LogLine {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    flags: u8,
    sp: u8,
    ppu_cyc: u32,
    ppu_sl: u32,
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
    map_res(
        take_while(|c:char| c.is_digit(10)),
        FromStr::from_str
    )(input)
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
        tag!("PPU:") >> ppu_cyc: ws!(d_u32) >> tag!(",") >> ppu_sl: ws!(d_u32) >>
        tag!("CYC:") >> cpu_cyc: ws!(d_u32) >>
        (
            LogLine {
                pc,
                a,
                x,
                y,
                flags,
                sp,
                ppu_cyc,
                ppu_sl,
                cpu_cyc
            }
        )
    )
);

fn parse_log() -> Result<Vec<LogLine>, std::io::Error> {
    let log = std::fs::read_to_string("roms/nes-test-roms/other/nestest.log")?;
    let lines = log.lines()
        .map(|line| {
            let (_, log_line) = parse_logline(line).expect(format!("invalid log line {}", line).as_str());
            log_line
        })
        .collect::<Vec<LogLine>>();

    Ok(lines)
}

fn assert_log(nes: &Nes, cpu_cyc: u64, line: &LogLine) {
    assert_eq!(cpu_cyc, line.cpu_cyc as u64, "incorrect cpu cycle at {}", line);
    assert_eq!(nes.cpu.pc.get(), line.pc, "incorrect pc at {}", line);
    assert_eq!(nes.cpu.acc.get(), line.a, "incorrect acc at {}", line);
    assert_eq!(nes.cpu.x.get(), line.x, "incorrect x at {}", line);
    assert_eq!(nes.cpu.y.get(), line.y, "incorrect y at {}", line);
    assert_eq!(nes.cpu.flags(), line.flags, "incorrect flags at {}", line);
    assert_eq!(nes.cpu.sp.get(), line.sp, "incorrect stack pointer at {}", line);
    // TODO: check ppu scanline and cycle
}

#[test]
fn test_nestest() {

    let log = parse_log().expect("cannot parse log");

    let cartridge = Cartridge::try_from(Path::new("roms/nes-test-roms/other/nestest.nes").to_owned()).unwrap();
    let nes = Nes::new(cartridge);

    let mut clock = nes.run(Some(0xC000));

    let mut log_iter = log.iter();

    // TODO: expose cpu_clock so we can read it here
    assert_log(&nes, 7, &log_iter.next().unwrap());
    loop {
        match Generator::resume(Pin::new(&mut clock), ()) {
            GeneratorState::Yielded(value) => {
                match value {
                    yaner::nes::NesCycle::CpuOp(cpu_cycle, _) => {
                        match log_iter.next() {
                            Some(line) => assert_log(&nes, cpu_cycle, line),
                            None => () // log is shorter than actual test
                        }
                    },
                    _ => ()
                }
            },
            GeneratorState::Complete(_) => break
        };
    }

    let result = nes.ram().read_u16(0x02);
    assert_eq!(0x00, result);
}
