use bitflags::bitflags;
use std::cell::Cell;

bitflags! {
    struct PpuStatus: u8 {
        const O = 1 << 5;
        const S = 1 << 6;
        const V = 1 << 7;
    }
}

pub struct Ppu {
    status: Cell<PpuStatus>
}

impl Ppu {
    pub fn new() -> Self {
        Ppu { status: Cell::new(PpuStatus::empty()) }
    }
    pub fn status(&self) -> u8 {
        let status = self.status.get();
        let mut cleared = PpuStatus::from(status);
        cleared.remove(PpuStatus::V);
        self.status.set(cleared);
        status.bits()
    }
}
