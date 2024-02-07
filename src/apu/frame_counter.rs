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

pub struct FrameCounter {
    register: Cell<Register>,
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
        [7457, 7456, 7458, 7457, 1, 1],
        [7457, 7456, 7458, 7458, 7452, 1],
    ];

    pub(super) fn new() -> Self {
        Self {
            register: Cell::default(),
            cycles: Cell::new(Self::NTSC_STEP_CYCLES[0][0]),
            step: Cell::default(),
        }
    }

    pub fn tick(&self) {
        self.cycles.update(|c| c - 1);
        let cycle = self.cycles.get();
        if cycle == 0 {
            self.step.update(|s| (s + 1) % 6);
            let step = self.step.get() as usize;
            let mode = self.register.get().mode() as usize;
            self.cycles.set(Self::NTSC_STEP_CYCLES[mode][step]);
        }
    }
}
