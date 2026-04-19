use bitregions::bitregions;

// https://www.nesdev.org/wiki/APU_Sweep
bitregions! {
    pub Sweep u8 {
        ENABLED: 0b1000_0000,
        PERIOD: 0b0111_0000,
        NEGATE: 0b0000_1000,
        SHIFT: 0b000_0111,
    }
}

impl Sweep {
    pub(super) fn tick(self) -> Self {
        self
    }
}
