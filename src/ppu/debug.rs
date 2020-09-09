use crate::ppu::{PpuCtrl, PpuMask, PpuStatus, Ppu};

pub struct PpuState {
    pub ctrl: PpuCtrl,
    pub mask: PpuMask,
    pub status: PpuStatus,
    pub oam_addr: u8,
    pub scanline: u16,
    pub dot: u16,
}

impl PpuState {
    pub fn new(ppu: &Ppu) -> Self {
        PpuState {
            ctrl: ppu.ctrl.get(),
            mask: ppu.mask.get(),
            status: ppu.status.get(),
            oam_addr: ppu.oam_addr.get(),
            scanline: ppu.scanline.get(),
            dot: ppu.dot.get(),
        }
    }
}
