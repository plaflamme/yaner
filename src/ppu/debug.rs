use crate::ppu::{Ppu, PpuCtrl, PpuMask};
use crate::ppu::renderer::{Pixel, PatternData, AttributeData};
use crate::ppu::reg::PpuStatus;
use crate::ppu::vram_address::VramAddress;
use crate::ppu::sprite::{Sprite, SpriteData};

pub struct PpuState {
    pub ctrl: PpuCtrl,
    pub mask: PpuMask,
    pub status: PpuStatus,
    pub t_addr: VramAddress,
    pub v_addr: VramAddress,
    pub fine_x: u8,
    pub oam_addr: u8,
    pub secondary_oam: [Option<Sprite>; 8],
    pub primary_oam: [Option<SpriteData>; 8],
    pub pattern_data: PatternData,
    pub attribute_data: AttributeData,
    pub frame: [Pixel; 256 * 240],
    pub scanline: u16,
    pub dot: u16,
}

impl PpuState {
    pub fn new(ppu: &Ppu) -> Self {
        PpuState {
            ctrl: ppu.registers.ctrl.get(),
            mask: ppu.registers.mask.get(),
            status: ppu.registers.status.get(),
            t_addr: ppu.registers.t_addr.get(),
            v_addr: ppu.registers.v_addr.get(),
            fine_x: ppu.registers.fine_x.get(),
            oam_addr: ppu.registers.oam_addr.get(),
            secondary_oam: ppu.renderer.secondary_oam.get(),
            primary_oam: ppu.renderer.primary_oam.get(),
            pattern_data: ppu.renderer.pattern_data.clone(),
            attribute_data: ppu.renderer.attribute_data.clone(),
            frame: ppu.renderer.frame_pixels.get(),
            scanline: ppu.renderer.scanline.get(),
            dot: ppu.renderer.dot.get(),
        }
    }
}
