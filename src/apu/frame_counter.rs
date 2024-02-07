use std::cell::Cell;

use bitflags::bitflags;

pub enum Mode {
    FourStep = 0,
    FiveStep = 1,
}

// https://www.nesdev.org/wiki/APU_Frame_Counter
bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct Register: u8 {
        const I = 1 << 6; // inhibit IRQ
        const M = 1 << 7; // mode
    }
}

impl Register {
    pub fn mode(&self) -> Mode {
        if self.contains(Register::M) {
            Mode::FiveStep
        } else {
            Mode::FourStep
        }
    }

    pub fn inhibit_irq(&self) -> bool {
        self.contains(Register::I)
    }
}

pub enum FrameType {
    Half,
    Quarter,
}

pub struct Clock {
    pub frame_type: FrameType,
    pub raise_interrupt: bool,
}

pub struct FrameCounter {
    register: Cell<Register>,
    bufferred: Cell<Option<(u8, Register)>>,
    cycles: Cell<u16>,
    step: Cell<u16>,
}

impl FrameCounter {
    // https://www.nesdev.org/wiki/APU_Frame_Counter
    // The APU is clocked on half-CPU cycles, but because it acutally acts on half clock ticks,
    //   we use the same cpu cycle here instead.
    // So the numbers in the chart are thus (for 4-step mode):
    // | APU Cycles | CPU Cycles | Delta |
    // | ---------- | ---------- | ----- |
    // |  3728.5    |   7457     | 7457  |
    // |  7456.5    |   14913    | 7456  |
    // |  11185.5   |   22371    | 7458  |
    // |  14914     |   29828    | 7457  |
    // |  14914.5   |   29829    | 1     |
    // |  14915     |   29830    | 1     |

    const NTSC_STEP_CYCLES: [[u16; 6]; 2] = [
        [7457, 7456, 7458, 7457, 1, 1],    // FourStep
        [7457, 7456, 7458, 7458, 7452, 1], // FiveStep
    ];

    pub(super) fn new() -> Self {
        Self {
            register: Cell::default(),
            bufferred: Cell::default(),
            cycles: Cell::new(Self::NTSC_STEP_CYCLES[Mode::FourStep as usize][0]),
            step: Cell::default(),
        }
    }

    fn set_state(&self, value: Register) {
        self.register.set(value);
        self.step.set(0);
        self.cycles
            .set(Self::NTSC_STEP_CYCLES[value.mode() as usize][0]);
    }

    pub fn write(&self, value: u8) {
        let value = Register::from_bits_truncate(value);
        // If the write occurs during an APU cycle, the effects occur 3 CPU cycles after the $4017 write cycle, and if the write occurs between APU cycles, the effects occurs 4 CPU cycles after the write cycle.
        let cycles = self.cycles.get();
        let delay = if cycles & 0x01 == 1 { 4 } else { 3 };
        self.bufferred.set(Some((delay, value)));
    }

    fn cycle(&self) -> Option<Clock> {
        self.cycles.update(|c| c - 1);
        let cycle = self.cycles.get();
        if cycle == 0 {
            self.step.update(|s| (s + 1) % 6);
            let step = self.step.get() as usize;
            let state = self.register.get();
            self.cycles
                .set(Self::NTSC_STEP_CYCLES[state.mode() as usize][step]);

            // From Mode 0: 4-Step Sequence (bit 7 of $4017 clear)
            // Due to half-cycles, there are 6 steps (0-5) in each mode
            //
            // 4-step mode:
            // - - - f f f   Interrupt
            // - l - - l -   Length counter and sweep (aka HalfFrame)
            // e e e - e -   Envelope and linear counter (aka QuarterFrame)
            //
            // 5-step mode:
            // - - - - - -   Interrupt (always disabled)
            // - l - - l -   Length counter and sweep (aka HalfFrame)
            // e e e - e -   Envelope and linear counter (aka QuarterFrame)
            let frame_type = match step {
                0 | 2 => FrameType::Quarter,
                1 | 4 => FrameType::Half,
                _ => return None,
            };

            let raise_interrupt =
                !state.inhibit_irq() && matches!(state.mode(), Mode::FourStep) && step >= 3;

            Some(Clock {
                frame_type,
                raise_interrupt,
            })
        } else {
            None
        }
    }

    pub fn tick(&self) -> Option<Clock> {
        let clock = self.cycle();
        if let Some((delay, value)) = self.bufferred.take() {
            if delay - 1 == 0 {
                self.set_state(value);

                // If the mode flag is set, then both "quarter frame" and "half frame" signals are also generated.
                if matches!(value.mode(), Mode::FiveStep) {
                    // Note this can't generate a Clock, but the previous can.
                    self.cycle();
                    // TODO: we need to generate another quarter frame here...
                    // but this won't generate an extra one, it will inhibit the other one if any
                    return Some(Clock {
                        frame_type: FrameType::Quarter,
                        raise_interrupt: false,
                    });
                }
            } else {
                self.bufferred.set(Some((delay - 1, value)))
            }
        }
        clock
    }
}
