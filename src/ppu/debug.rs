use crate::ppu::reg::PpuStatus;
use crate::ppu::renderer::{AttributeData, PatternData, Pixel, SpritePipeline};
use crate::ppu::sprite::{Sprite, SpriteData};
use crate::ppu::vram_address::VramAddress;
use crate::ppu::{Ppu, PpuCtrl, PpuMask};

pub struct SpritePipelineState {
    pub oam_entry: u8,
    pub secondary_oam_index: u8,
    pub secondary_oam: [u8;32],
    pub output_units: [Option<SpriteData>;8],
}

impl SpritePipelineState {
    fn new(p: &SpritePipeline) -> Self {
        SpritePipelineState {
            oam_entry: p.oam_entry.get(),
            secondary_oam_index: p.secondary_oam_index.get(),
            secondary_oam: p.secondary_oam.get(),
            output_units: p.sprite_output_units.get(),
        }
    }
}

pub struct PpuState {
    pub ctrl: PpuCtrl,
    pub mask: PpuMask,
    pub status: PpuStatus,
    pub t_addr: VramAddress,
    pub v_addr: VramAddress,
    pub fine_x: u8,
    pub oam_addr: u8,
    pub sprite_pipeline: SpritePipelineState,
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
            sprite_pipeline: SpritePipelineState::new(&ppu.renderer.sprite_pipeline),
            pattern_data: ppu.renderer.pattern_data.clone(),
            attribute_data: ppu.renderer.attribute_data.clone(),
            frame: ppu.renderer.frame_pixels.get(),
            scanline: ppu.renderer.scanline.get(),
            dot: ppu.renderer.dot.get(),
        }
    }
}
