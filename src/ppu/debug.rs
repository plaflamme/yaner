use crate::ppu::{Ppu, PpuCtrl, PpuMask, PpuStatus, VramAddress};

pub struct PpuState {
    pub ctrl: PpuCtrl,
    pub mask: PpuMask,
    pub status: PpuStatus,
    pub t_addr: VramAddress,
    pub v_addr: VramAddress,
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
            t_addr: ppu.t_addr.get(),
            v_addr: ppu.v_addr.get(),
            oam_addr: ppu.oam_addr.get(),
            scanline: ppu.scanline.get(),
            dot: ppu.dot.get(),
        }
    }
}
