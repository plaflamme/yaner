use std::cell::Cell;

use bitflags::bitflags;

#[derive(Debug, PartialEq, Eq)]
pub enum Mode {
    FourStep = 0,
    FiveStep = 1,
}

// https://www.nesdev.org/wiki/APU_Frame_Counter
bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct Status: u8 {
        const I = 1 << 6; // inhibit IRQ
        const M = 1 << 7; // mode: 0 == FourStep, 1 == FiveStep
    }
}

impl Status {
    pub fn mode(&self) -> Mode {
        if self.contains(Status::M) {
            Mode::FiveStep
        } else {
            Mode::FourStep
        }
    }

    pub fn inhibit_irq(&self) -> bool {
        self.contains(Status::I)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FrameType {
    Half,
    Quarter,
}

pub struct Clock {
    pub frame_type: Option<FrameType>,
    pub raise_interrupt: bool,
}

pub struct FrameCounter {
    status: Cell<Status>,
    bufferred: Cell<Option<(u8, Status)>>,
    cpu_cycles: Cell<u64>,
    cycles: Cell<u16>,
    step: Cell<u8>,
    irq_flag: Cell<bool>,
}

impl FrameCounter {
    // https://www.nesdev.org/wiki/APU_Frame_Counter
    const NTSC_STEP_CYCLES: [[u16; 6]; 2] = [
        [7457, 14913, 22371, 29828, 29829, 29830], // FourStep
        [7457, 14913, 22371, 29829, 37281, 37282], // FiveStep
    ];

    const FRAME_TYPES: [Option<FrameType>; 6] = [
        Some(FrameType::Quarter),
        Some(FrameType::Half),
        Some(FrameType::Quarter),
        None,
        Some(FrameType::Half),
        None,
    ];

    pub(super) fn new() -> Self {
        Self {
            status: Cell::default(),
            bufferred: Cell::default(),
            cpu_cycles: Cell::default(),
            cycles: Cell::default(),
            step: Cell::default(),
            irq_flag: Cell::default(),
        }
    }

    // Reads the
    pub(crate) fn irq_flag(&self) -> bool {
        // Reading the IRQ flag disables it
        self.irq_flag.replace(false)
    }

    pub fn write(&self, value: u8) {
        let value = Status::from_bits_truncate(value);
        if value.inhibit_irq() {
            self.irq_flag.set(false);
        }
        // If the write occurs during an APU cycle, the effects occur 3 CPU cycles after the $4017 write cycle, and if the write occurs between APU cycles, the effects occurs 4 CPU cycles after the write cycle.
        let cycles = self.cpu_cycles.get();
        let delay = if cycles & 0x01 == 1 { 4 } else { 3 };
        self.bufferred.set(Some((delay, value)));
        log::debug!("write status={value:?} cpu_cycles={cycles} delay={delay}");
    }

    fn set_state(&self, value: Status) {
        self.status.set(value);
        self.step.set(0);
        self.cycles.set(0);
        log::debug!("set_state: status={:?} step=0 cycles=0", self.status.get());
    }

    fn cycle(&self) -> Clock {
        self.cpu_cycles.update(|c| c + 1);
        self.cycles.update(|c| c + 1);
        let cycle = self.cycles.get();
        let state = self.status.get();
        let current_step = self.step.get();
        // The next cycle number when something needs to happen
        let next_cycle = Self::NTSC_STEP_CYCLES[state.mode() as usize][current_step as usize];

        let frame_type = if cycle != next_cycle {
            None
        } else {
            let frame_type = Self::FRAME_TYPES[current_step as usize];
            let raise_interrupt =
                !state.inhibit_irq() && state.mode() == Mode::FourStep && current_step >= 3;
            if raise_interrupt {
                self.irq_flag.set(true);
            }
            log::debug!(
                "tick: step={current_step} cycles={cycle} frame_type={frame_type:?} intr={raise_interrupt}",
            );
            let current_step = (current_step + 1) % 6;
            self.step.set(current_step);
            if current_step == 0 {
                self.cycles.set(0);
            }
            log::debug!("tick: current_step={:?} step=0 cycles=0", self.status.get());

            frame_type
        };

        Clock {
            frame_type,
            raise_interrupt: self.irq_flag.get(),
        }
    }

    pub fn tick(&self) -> Clock {
        let clock = self.cycle();
        if let Some((delay, value)) = self.bufferred.take() {
            if delay - 1 == 0 {
                self.set_state(value);
            } else {
                self.bufferred.set(Some((delay - 1, value)))
            }
        }
        clock
    }
}

pub struct FrameCounterState {
    pub status: Status,
    pub cycles: u16,
    pub step: u8,
}

impl FrameCounterState {
    pub fn new(fc: &FrameCounter) -> Self {
        Self {
            status: fc.status.get(),
            cycles: fc.cycles.get(),
            step: fc.step.get(),
        }
    }
}
