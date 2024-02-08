use bitflags::bitflags;
use std::{cell::Cell, ops::Coroutine};

mod frame_counter;

use frame_counter::FrameCounter;

use crate::{cpu::bus::Irq, memory::AddressSpace};

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

pub enum ApuCycle {
    Tick,
}

pub struct Apu {
    status: Cell<Status>,
    frame_counter: FrameCounter,
    irqs: Cell<Irq>,
}

impl Apu {
    pub fn new() -> Self {
        Self {
            status: Cell::default(),
            frame_counter: FrameCounter::new(),
            irqs: Cell::default(),
        }
    }

    pub fn irqs(&self) -> Irq {
        self.irqs.get()
    }

    pub fn run(&self) -> impl Coroutine<Yield = ApuCycle, Return = ()> + '_ {
        move || loop {
            if let Some(clock) = self.frame_counter.tick() {
                self.irqs.update(|f| {
                    if clock.raise_interrupt {
                        f | Irq::FrameCounter
                    } else {
                        f - Irq::FrameCounter
                    }
                });
            }
            yield ApuCycle::Tick
        }
    }
}

impl AddressSpace for Apu {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x4015 => self.status.get().bits(),
            _ => todo!(),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x4017 => self.frame_counter.write(value),
            _ => (),
        }
    }
}
