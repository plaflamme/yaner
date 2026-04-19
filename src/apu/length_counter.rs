pub const LENGTHS: [u8; 32] = [
    0x0a, 0xfe, 0x14, 0x02, 0x28, 0x04, 0x50, 0x06, 0xa0, 0x08, 0x3c, 0x0a, 0x0e, 0x0c, 0x1a, 0x0e,
    0x0c, 0x10, 0x18, 0x12, 0x30, 0x14, 0x60, 0x16, 0xc0, 0x18, 0x48, 0x1a, 0x10, 0x1c, 0x20, 0x1e,
];

#[derive(Clone, Copy, Default)]
pub(super) struct LengthCounter {
    enabled: bool,
    halted: bool,
    counter: u8,
}

impl LengthCounter {
    pub(super) fn enable(mut self, enabled: bool) -> Self {
        // When the enabled bit is cleared (via $4015), the length counter is forced to 0 and cannot be changed until enabled is set again (the length counter's previous value is lost). There is no immediate effect when enabled is set.
        if !enabled {
            self.counter = 0;
        }
        self.enabled = enabled;
        self
    }

    pub(super) fn halt(mut self, halted: bool) -> Self {
        self.halted = halted;
        self
    }

    pub(crate) fn load(mut self, value: u8) -> Self {
        if self.enabled {
            self.counter = LENGTHS[value as usize];
        }
        self
    }

    pub(super) fn tick(mut self) -> Self {
        if !self.halted && self.counter > 0 {
            self.counter -= 1;
        }
        self
    }

    pub(super) fn playing(&self) -> bool {
        self.counter > 0
    }
}
