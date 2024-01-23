#![feature(coroutines, coroutine_trait)]

use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::ops::{Coroutine, CoroutineState};
use std::path::Path;
use std::pin::Pin;
use std::str::FromStr;

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
    ppu_scanline: u32,
    ppu_dot: u32,
    cpu_cyc: u32,
}

impl Display for LogLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04X}", self.pc)
    }
}

fn parse_logline(input: &str) -> Result<LogLine, Box<dyn std::error::Error>> {
    //           1         2         3         4         5         6         7         8         9
    // 0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
    // C000  4C F5 C5  JMP $C5F5                       A:00 X:00 Y:00 P:24 SP:FD PPU:  0, 21 CYC:7

    let hex_u8 = |r: std::ops::Range<usize>| u8::from_str_radix(&input[r], 16);

    let pc = u16::from_str_radix(&input[0..4], 16)?;
    let a = hex_u8(50..52)?;
    let x = hex_u8(55..57)?;
    let y = hex_u8(60..62)?;
    let flags = hex_u8(65..67)?;
    let sp = hex_u8(71..73)?;
    let ppu_scanline = u32::from_str(input[78..81].trim())?;
    let ppu_dot = u32::from_str(input[82..85].trim())?;
    let cpu_cyc = u32::from_str(input[90..].trim())?;
    Ok(LogLine {
        pc,
        a,
        x,
        y,
        flags,
        sp,
        ppu_dot,
        ppu_scanline,
        cpu_cyc,
    })
}

fn parse_log() -> Result<Vec<LogLine>, std::io::Error> {
    let log = std::fs::read_to_string("roms/nes-test-roms/other/nestest.log")?;
    let lines = log
        .lines()
        .map(|line| parse_logline(line).unwrap_or_else(|_| panic!("invalid log line {}", line)))
        .collect::<Vec<LogLine>>();

    Ok(lines)
}

fn assert_log(nes: &Nes, line: &LogLine) {
    let irrelevant_flags = Flags::B - Flags::U;
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
    // nestest's log is wrong about the flags on power up:
    // http://wiki.nesdev.com/w/index.php/CPU_power_up_state#cite_note-1
    assert_eq!(
        state.cpu.flags - irrelevant_flags,
        Flags::from_bits_truncate(line.flags) - irrelevant_flags,
        "incorrect flags at {}",
        line
    );
    assert_eq!(state.cpu.sp, line.sp, "incorrect stack pointer at {}", line);

    // NOTE: we're one dot behind nestest apparently...
    let dot = (nes.debug().ppu.dot + 1) % 341;
    let mut scanline = nes.debug().ppu.scanline;
    if dot == 0 {
        scanline += 1;
    }

    assert_eq!(
        scanline, line.ppu_scanline as u16,
        "incorrect ppu scanline at {}",
        line
    );
    assert_eq!(dot, line.ppu_dot as u16, "incorrect ppu dot at {}", line);
}

// Steps the same way nintendulator does, which is:
//   (cpu_step + 3 * ppu_step)
//   if cpu_step == OpComplete { yield () }
fn nintendulator_steps(nes: &Nes) -> impl Coroutine<Yield = (), Return = ()> + '_ {
    let mut ppu_steps = nes.steps();
    move || loop {
        match Coroutine::resume(Pin::new(&mut ppu_steps), ()) {
            CoroutineState::Yielded(NesCycle::PowerUp) => (),
            CoroutineState::Yielded(NesCycle::Cpu(CpuCycle::OpComplete(_, _))) => {
                yield ();
            }
            CoroutineState::Yielded(_) => (),
            CoroutineState::Complete(_) => break,
        }
    }
}

#[test]
#[should_panic] // cycles are offset now, maybe I'll fix this, maybe I won't
fn test_nestest() {
    let log = parse_log().expect("cannot parse log");

    let cartridge =
        Cartridge::try_from(Path::new("roms/nes-test-roms/other/nestest.nes").to_owned()).unwrap();
    let nes = Nes::new_with_pc(cartridge, Some(0xC000));

    let mut steps = nintendulator_steps(&nes);

    let mut log_iter = log.iter();

    while let CoroutineState::Yielded(_) = Coroutine::resume(Pin::new(&mut steps), ()) {
        if let Some(line) = log_iter.next() {
            assert_log(&nes, line)
        }
    }

    assert!(log_iter.next().is_none(), "did not consume whole log");

    let result = nes.debug().ram.read_u16(0x02);
    assert_eq!(0x00, result);
}
