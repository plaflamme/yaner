use std::cell::Cell;

use crate::{
    apu::{frame_counter::FrameType, length_counter::LengthCounter, sweep::Sweep},
    memory::AddressSpace,
};
use bitregions::bitregions;

// https://www.nesdev.org/wiki/APU_Pulse

bitregions! {
    pub Status u8 {
        DUTY: 0b1100_0000,
        LENGTH_COUNTER_HALT: 0b0010_0000,
        CONSTANT_VOLUME: 0b0001_0000,
        VOLUME: 0b0000_1111,
    }
}

bitregions! {
    pub Load u8 {
        LENGTH_COUNTER: 0b1111_1000,
        TIMER_HIGH: 0b0000_0111,
    }
}

pub(super) struct Pulse {
    status: Cell<Status>,
    sweep: Cell<Sweep>,
    length_counter: Cell<LengthCounter>,
}

impl Pulse {
    pub(super) fn new() -> Self {
        Self {
            status: Cell::default(),
            sweep: Cell::default(),
            length_counter: Cell::default(),
        }
    }

    pub(super) fn tick(&self, frame_type: FrameType) {
        match frame_type {
            FrameType::Half => {
                self.sweep.update(|sweep| sweep.tick());
                self.length_counter.update(|lc| lc.tick());
            }
            FrameType::Quarter => (),
        }
        log::debug!("tick length_counter={:?}", self.length_counter.get());
    }

    pub(super) fn playing(&self) -> bool {
        self.length_counter.get().playing()
    }

    pub(super) fn enable_length_counter(&self, enabled: bool) {
        self.length_counter.update(|lc| lc.enable(enabled));
    }
}

impl AddressSpace for Pulse {
    fn read_u8(&self, _addr: u16) -> u8 {
        // the addresses below are write-only! Reading from these addresses exhibits open-bus behavior.
        0 // TODO: OpenBus behaviour
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr % 4 {
            0x00 => {
                let status = Status::from(value);
                log::debug!("set_state status={status:?}");
                self.status.set(status);
                self.length_counter
                    .update(|lc| lc.halt(status.length_counter_halt()));
            }
            0x01 => self.sweep.set(Sweep::from(value)),
            0x02 => (), // TODO: timer low
            0x03 => {
                let load = Load::from(value);
                log::debug!(
                    "load length_counter={:02X} timer_high={:02X}",
                    load.length_counter(),
                    load.timer_high()
                );
                self.length_counter
                    .update(|lc| lc.load(load.length_counter()));
            }
            _ => unreachable!(),
        }
    }
}
