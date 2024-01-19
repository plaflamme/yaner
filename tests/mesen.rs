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

//

#[derive(Debug)]
struct LogLine {
    idx: usize,
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    flags: u8,
    ppu_scanline: u16,
    ppu_dot: u32,
    cpu_cyc: u32,
}

impl Display for LogLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:04X}", self.idx + 1, self.pc)
    }
}

fn parse_logline(input: &str, idx: usize) -> Result<LogLine, Box<dyn std::error::Error>> {
    //           1         2         3         4         5         6
    // 012345678901234567890123456789012345678901234567890123456789012345678
    // E681  SEI                A:00 X:00 Y:00 S:FD P:04 V:0   H:27  Cycle:8

    let hex_u8 = |r: std::ops::Range<usize>| u8::from_str_radix(&input[r], 16);

    let pc = u16::from_str_radix(&input[0..4], 16)?;
    let a = hex_u8(27..29)?;
    let x = hex_u8(32..34)?;
    let y = hex_u8(37..39)?;
    let sp = hex_u8(42..44)?;
    let flags = hex_u8(47..49)?;
    let ppu_scanline = i32::from_str(input[52..56].trim())?;
    let ppu_dot = u32::from_str(input[58..62].trim())?;
    let cpu_cyc = u32::from_str(input[68..].trim())?;
    Ok(LogLine {
        idx,
        pc,
        a,
        x,
        y,
        flags,
        sp,
        ppu_dot,
        ppu_scanline: if ppu_scanline == -1 {
            261
        } else {
            ppu_scanline as u16
        },
        cpu_cyc,
    })
}

fn parse_log() -> Result<Vec<LogLine>, std::io::Error> {
    let log = std::fs::read_to_string("tests/05-nmi_timing.txt")?;
    let lines = log
        .lines()
        .enumerate()
        .map(|(idx, line)| {
            parse_logline(line, idx).unwrap_or_else(|_| panic!("invalid log line@{idx}: {line}"))
        })
        .collect::<Vec<LogLine>>();

    Ok(lines)
}

fn assert_log(nes: &Nes, line: &LogLine) {
    let irrelevant_flags = Flags::B | Flags::U;
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
    assert_eq!(
        nes.debug().ppu.scanline,
        line.ppu_scanline,
        "incorrect ppu scanline at {}",
        line
    );
    assert_eq!(
        nes.debug().ppu.dot,
        line.ppu_dot as u16,
        "incorrect ppu dot at {}",
        line
    );
}

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
#[should_panic]
fn test_mesen() {
    let log = parse_log().expect("cannot parse log");

    let cartridge = Cartridge::try_from(
        Path::new("roms/nes-test-roms/ppu_vbl_nmi/rom_singles/05-nmi_timing.nes").to_owned(),
    )
    .unwrap();
    let nes = Nes::new(cartridge);

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
