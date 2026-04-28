use bitflags::bitflags;
use std::{cell::Cell, ops::Coroutine};

pub mod debug;
mod envelope;
pub mod frame_counter;
mod length_counter;
pub mod pulse;
mod sequencer;
mod sweep;

use frame_counter::FrameCounter;
use pulse::Pulse;

use crate::{apu::frame_counter::FrameType, memory::AddressSpace};

// https://www.nesdev.org/wiki/APU_Frame_Counter
bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct Status: u8 {
        const P1 = 1 << 0; // Pulse unit 1 length counter
        const P2 = 1 << 1; // Pulse unit 2 length counter
        const T = 1 << 2; // Triangle unit length counter
        const N = 1 << 3; // Noise unit length counter
        const D = 1 << 4; // DMC Active
        const F = 1 << 6; // Frame counter interrupt
        const I = 1 << 7; // DMC interrupt
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CpuCycle {
    Get,
    Put,
}
impl CpuCycle {
    fn next(&self) -> Self {
        match self {
            CpuCycle::Get => CpuCycle::Put,
            CpuCycle::Put => CpuCycle::Get,
        }
    }
}

pub enum ApuCycle {
    Tick { irq: bool },
}

pub struct Apu {
    // Represents the current CPU cycle, the one we haven't caught up to yet
    cpu_cycle: Cell<CpuCycle>,
    pulse_1: Pulse,
    pulse_2: Pulse,
    frame_counter: FrameCounter,
}

impl Apu {
    pub fn new() -> Self {
        Self {
            cpu_cycle: Cell::new(CpuCycle::Get),
            frame_counter: FrameCounter::new(),
            pulse_1: Pulse::new(),
            pulse_2: Pulse::new(),
        }
    }

    fn handle_frame(&self, frame_type: FrameType) {
        log::debug!("handle_frame: frame_type={frame_type:?}");
        self.pulse_1.tick(frame_type);
        self.pulse_2.tick(frame_type);
    }

    fn status(&self) -> Status {
        let mut status = Status::default();
        status.set(Status::F, self.frame_counter.irq_flag(self.cpu_cycle.get()));
        status.set(Status::P1, self.pulse_1.playing());
        status.set(Status::P2, self.pulse_2.playing());
        status
    }

    pub fn run(&self) -> impl Coroutine<Yield = ApuCycle, Return = ()> + '_ {
        #[coroutine]
        move || loop {
            let clock = self.frame_counter.tick(self.cpu_cycle.get());
            if let Some(frame_type) = clock.frame_type {
                self.handle_frame(frame_type);
            }
            self.cpu_cycle.update(|c| c.next());
            yield ApuCycle::Tick {
                irq: clock.raise_interrupt,
            };
        }
    }
}

impl Default for Apu {
    fn default() -> Self {
        Self::new()
    }
}

impl AddressSpace for Apu {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x4000..=0x4003 => self.pulse_1.read_u8(addr),
            0x4004..=0x4007 => self.pulse_2.read_u8(addr),
            0x4015 => {
                let status = self.status();
                log::debug!("read 0x4015: {status:?}");
                status.bits()
            }
            _ => todo!(),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x4000..=0x4003 => self.pulse_1.write_u8(addr, value),
            0x4004..=0x4007 => self.pulse_2.write_u8(addr, value),
            0x4015 => {
                let status = Status::from_bits_truncate(value);
                log::debug!("write 0x4015: {status:?}");
                self.pulse_1
                    .enable_length_counter(status.contains(Status::P1));
                self.pulse_2
                    .enable_length_counter(status.contains(Status::P2));
            }
            0x4017 => {
                log::debug!("write 0x4017: {value:02X}");
                self.frame_counter.write(self.cpu_cycle.get(), value);
                if value & 0x80 != 0 {
                    // Writing to $4017 with bit 7 set ($80) will immediately clock all of its controlled units at the beginning of the 5-step sequence
                    self.handle_frame(FrameType::Half);
                }
            }
            _ => (),
        }
    }
}
