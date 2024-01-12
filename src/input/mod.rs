use std::cell::Cell;

use bitflags::bitflags;

pub trait Input {
    fn strobe(&self, value: u8);

    fn read(&self) -> u8;
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct JoypadButtons: u8 {
        const A = 1 << 0;
        const B = 1 << 1;
        const Select = 1 << 2;
        const Start = 1 << 3;
        const Up = 1 << 4;
        const Down = 1 << 5;
        const Left = 1 << 6;
        const Right = 1 << 7;
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
        self.state.set(state.bits());
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
            _ => panic!("invalid strobe value"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_joypad_strobe_high() {
        // when strobe is high, reading should always return the first bit (button A)
        let joypad = Joypad::default();
        joypad.update(JoypadButtons::A);
        joypad.strobe(1);
        assert_eq!(1, joypad.read());
        assert_eq!(1, joypad.read());
        joypad.update(JoypadButtons::empty());
        assert_eq!(0, joypad.read());
        assert_eq!(0, joypad.read());
    }

    #[test]
    fn test_joypad_strobe_low() {
        // when strobe is high, reading should return values in the following order:
        // A, B, Select, Start, Up, Down, Left, Right
        let order = vec![
            JoypadButtons::A,
            JoypadButtons::B,
            JoypadButtons::Select,
            JoypadButtons::Start,
            JoypadButtons::Up,
            JoypadButtons::Down,
            JoypadButtons::Left,
            JoypadButtons::Right,
        ];
        for btn in order {
            let joypad = Joypad::default();
            joypad.strobe(1);
            joypad.update(btn);
            joypad.strobe(0);
            let mut bits = 0u8;
            for i in 0..8 {
                let value = joypad.read();
                bits |= value << i;
            }
            assert_eq!(btn, JoypadButtons::from_bits(bits).unwrap());
        }
    }

    #[test]
    fn test_joypad_read_multiple() {
        let multiple =
            JoypadButtons::A | JoypadButtons::Select | JoypadButtons::Up | JoypadButtons::Left;
        let joypad = Joypad::default();
        joypad.strobe(1);
        joypad.update(multiple);
        joypad.strobe(0);
        let mut bits = 0u8;
        for i in 0..8 {
            let value = joypad.read();
            bits |= value << i;
        }
        assert_eq!(multiple, JoypadButtons::from_bits(bits).unwrap());
    }

    #[test]
    fn test_joypad_read_end() {
        let joypad = Joypad::default();
        joypad.strobe(1);
        joypad.update(JoypadButtons::empty());
        joypad.strobe(0);
        let mut bits = 0u8;
        for i in 0..8 {
            let value = joypad.read();
            bits |= value << i;
        }
        assert_eq!(0, bits);
        // reading from this point should always return 1
        assert_eq!(1, joypad.read());
        assert_eq!(1, joypad.read());
    }
}
