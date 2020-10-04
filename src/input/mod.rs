use std::cell::Cell;

use bitflags::bitflags;

pub trait Input {

    fn strobe(&self, value: u8);

    fn read(&self) -> u8;

}

bitflags! {
    pub struct JoypadButtons: u8 {
        const A = 1 << 0;
        const B = 1 << 1;
        const SELECT = 1 << 2;
        const START = 1 << 3;
        const UP = 1 << 4;
        const DOWN = 1 << 5;
        const LEFT = 1 << 6;
        const RIGHT = 1 << 7;
    }
}

// http://wiki.nesdev.com/w/index.php/Standard_controller
#[derive(Default)]
pub struct Joypad {
    strobe: Cell<u8>,

    // NOTE: implement a ShiftRegister if this needs to be shared between multiple kinds of inputs
    state: Cell<u8>,
    cursor: Cell<u8>,
}

impl Joypad {
    pub fn update(&self, state: JoypadButtons) {
        self.state.set(state.bits);
    }
}

impl Input for Joypad {

    fn strobe(&self, value: u8) {
        self.strobe.set(value & 0x1);
        if value & 0x1 == 1 {
            self.cursor.set(0);
        }
    }

    fn read(&self) -> u8 {
        match self.strobe.get() {
            1 => self.state.get() & 0x01,
            0 => {
                let bit = self.cursor.get();
                self.cursor.update(|c| c.saturating_add(1));
                match bit {
                    0..=7 => (self.state.get() >> bit) & 0x1,
                    _ => 1,
                }
            }
            _ => panic!("invalid strobe value")
        }
    }
}
