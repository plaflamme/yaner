use std::{ops::Coroutine, rc::Rc};

mod frame_counter;

use frame_counter::FrameCounter;

use crate::memory::AddressSpace;

pub enum ApuCycle {
    Tick { irq: bool },
}

pub struct Apu {
    frame_counter: FrameCounter,
}

impl Apu {
    pub fn new() -> Self {
        Self {
            frame_counter: FrameCounter::new(),
        }
    }

    pub fn run(&self) -> impl Coroutine<Yield = ApuCycle, Return = ()> + '_ {
        #[coroutine]
        move || loop {
            if let Some(clock) = self.frame_counter.tick() {
                yield ApuCycle::Tick {
                    irq: clock.raise_interrupt,
                }
            } else {
                yield ApuCycle::Tick { irq: false }
            }
        }
    }
}

impl AddressSpace for Apu {
    fn read_u8(&self, addr: u16) -> u8 {
        todo!()
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x4017 => self.frame_counter.write(value),
            _ => (),
        }
    }
}
