use bitregions::bitregions;
use crate::ppu::reg::{PpuStatus, Registers};
use crate::memory::AddressSpace;
use std::cell::{Cell, RefCell};

bitregions! {
    pub Attributes u8 {
        PALETTE: 0b0000_0011,
        PRIORITY: 0b0010_0000, // Priority (0: in front of background; 1: behind background)
        FLIP_H: 0b0100_0000,
        FLIP_V: 0b1000_0000,
    }
}

impl Attributes {
    pub fn bg_priority(&self) -> bool {
        self.priority()
    }
    pub fn fg_priority(&self) -> bool {
        !self.priority()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Sprite {
    pub id: u8, // for sprite priority
    pub x: u8,
    pub y: u8,
    pub tile_index: u8,
    pub attr: Attributes,
}

impl Sprite {
    pub fn new(id: u8, values: [u8; 4]) -> Self {
        Sprite {
            id,
            y: values[0],
            tile_index: values[1],
            attr: Attributes::new(values[2]),
            x: values[3],
        }
    }
}

#[derive(Clone, Copy)]
pub struct SpriteData {
    pub sprite: Sprite,
    pub tile_low: u8,
    pub tile_high: u8,
}

impl SpriteData {
    pub fn new(sprite: Sprite, tile_low: u8, tile_high: u8) -> Self {
        SpriteData {
            sprite,
            tile_low,
            tile_high,
        }
    }
}

bitregions! {
    pub OamAddr u8 {
        LOW: 0b0000_0011,
        HIGH: 0b1111_1100,
    }
}

impl OamAddr {
    // increments the low part of the address, returns true if we wrapped around
    fn incr_low(&mut self) -> bool {
        let low = (self.low() + 1) & OamAddr::LOW;
        self.set_low(low);
        low == 0
    }
    // increments the high part of the address, returns true if we wrapped around
    fn incr_high(&mut self) -> bool {
        let high = (self.high() + 1) & 0b0011_1111;
        self.set_high(high);
        high == 0
    }
}

#[derive(Default)]
pub(super) struct SpritePipeline {
    // The sprite data to render on the current scanline
    sprite_output_units: RefCell<Vec<SpriteData>>,

    secondary_oam: Cell<[u8; 32]>,
    secondary_oam_index: Cell<u8>,

    // This is a different representation of PpuCtrl::OamAddr so we can increment different parts of it independently.
    oam_addr: Cell<OamAddr>,
    // Value read from main OAM on odd cycles
    oam_entry: Cell<u8>,
    // Whether we've completed the evaluation
    oam_done: Cell<bool>,
    // Whether the current sprite was in range when we looked at its y coordinate
    sprite_in_range: Cell<bool>,
}

impl SpritePipeline {

    fn write_u8(&self, value: u8) {
        self.secondary_oam.update(|mut oam| {
            oam[self.secondary_oam_index.get() as usize] = value;
            oam
        });
    }

    fn clear(&self, index: usize) {
        self.secondary_oam.update(|mut oam| {
            oam[index] = 0xFF;
            oam
        });
    }

    fn is_full(&self) -> bool {
        self.secondary_oam_index.get() >= 32
    }

    fn sprite_count(&self) -> u8 {
        // the index points to the 0th byte of the next sprite, so we divide by 4 to get the number of sprites.
        self.secondary_oam_index.get() >> 2
    }

    pub fn reset_output_units(&self) {
        self.sprite_output_units.borrow_mut().clear();
    }

    // returns the sprites that overlap with the provided dot (x coordinate)
    pub fn sprite_output_at(&self, dot: u16) -> Vec<SpriteData> {
        self.sprite_output_units.borrow()
            .iter()
            .filter(|sprite_data| {
                let sprite_x = sprite_data.sprite.x as u16;
                let sprite_end_x = sprite_x + 8;
                dot >= sprite_x && dot < sprite_end_x
            })
            .cloned()
            .collect()
    }

    pub fn cycle(
        &self,
        scanline: u16,
        dot: u16,
        registers: &Registers,
        oam_ram: &dyn AddressSpace,
        bus: &dyn AddressSpace,
    ) {
        match dot {
            1..=64 => {
                // "Cycles 1-64: Secondary OAM (32-byte buffer for current sprites on scanline) is initialized to $FF - attempting to read $2004 will return $FF.
                // Internally, the clear operation is implemented by reading from the OAM and writing into the secondary OAM as usual, only a signal is active that makes the read always return $FF."
                self.clear((dot - 1) as usize >> 1);
            }
            65..=256 => {
                if dot == 65 {
                    self.oam_addr.set(OamAddr::new(registers.oam_addr.get()));
                    self.secondary_oam_index.set(0);
                    self.sprite_in_range.set(false);
                    self.oam_done.set(false);
                }

                if dot & 0x01 != 0 {
                    // "On odd cycles, data is read from (primary) OAM"
                    self.oam_entry.set(oam_ram.read_u8(registers.oam_addr.get() as u16));
                } else if !self.oam_done.get() {
                    // "On even cycles, data is written to secondary OAM (unless secondary OAM is full, in which case it will read the value in secondary OAM instead)"

                    let entry = self.oam_entry.get();
                    let mut oam_addr = self.oam_addr.get();
                    let prev_oam_high = oam_addr.high();

                    let in_range = if self.sprite_in_range.get() { true } else {
                        let sprite_y = entry as u16;
                        let sprite_y_end = sprite_y + registers.ctrl.get().sprite_height() as u16;
                        self.sprite_in_range.update(|_| scanline >= sprite_y && scanline < sprite_y_end)
                    };

                    if !self.is_full() {
                        self.write_u8(entry);

                        if in_range {
                            self.secondary_oam_index.update(|s| s + 1);
                            // NOTE: Mesen tests against the index in secondary oam apparently to replicate some obscure corner case.
                            if oam_addr.incr_low() {
                                // finished copying this sprite's data, move to the next one.
                                self.sprite_in_range.set(false);
                                oam_addr.incr_high();
                            }
                        } else {
                            // nothing to do for this sprite, skip to the next one
                            oam_addr.incr_high();
                        }
                    } else {
                        // OAM is full
                        if in_range {
                            registers.status.update(|s| s | PpuStatus::O);

                            if oam_addr.incr_low() {
                                oam_addr.incr_high();
                            }
                        } else {
                            // This is the overflow bug where we increment both high and low (without carry)
                            oam_addr.incr_low();
                            oam_addr.incr_high();
                        }
                    }
                    // if we've wrapped around, we're done.
                    if prev_oam_high != 0 && oam_addr.high() == 0 {
                        self.oam_done.set(true);
                    }
                    self.oam_addr.set(oam_addr);
                    registers.oam_addr.set(oam_addr.raw());
                }
            }
            257..=320 => {
                //"OAMADDR is set to 0 during each of ticks 257-320 (the sprite tile loading interval) of the pre-render and visible scanlines." (When rendering)
                registers.oam_addr.set(0);

                let cycle = (dot - 257) % 8;
                let sprite_index = ((dot - 257) / 8) as usize;
                match cycle {
                    0 => if sprite_index < self.sprite_count() as usize {
                        let ctrl = registers.ctrl.get();
                        let oam = self.secondary_oam.get();
                        let base = sprite_index * 4;
                        let sprite_oam: [u8;4] = [
                            oam[base],
                            oam[base+1],
                            oam[base+2],
                            oam[base+3],
                        ];
                        let sprite = Sprite::new(0, sprite_oam);

                        let base_addr = if ctrl.large_sprites() {
                            let pattern_table = sprite.tile_index as u16 & 1 * 0x1000;
                            let addr = (sprite.tile_index & 0b1111_1110) as u16;
                            pattern_table | addr * 16
                        } else {
                            let pattern_table = ctrl.sprite_pattern_table_address();
                            let addr = sprite.tile_index as u16;
                            pattern_table | addr * 16
                        };

                        let sprite_height = ctrl.sprite_height() as u16;
                        let mut y_sprite = scanline - sprite.y as u16;
                        if sprite.attr.flip_v() {
                            y_sprite ^= sprite_height - 1;
                        }
                        // for large sprites, if y_sprite > 8, we must use the second tile
                        let second_tile_offset = y_sprite & 8;
                        let addr = base_addr + y_sprite + second_tile_offset;
                        let tile_low = bus.read_u8(addr);
                        let tile_high = bus.read_u8(addr + 8);
                        let sprite_data = SpriteData::new(sprite, tile_low, tile_high);

                        self.sprite_output_units.borrow_mut().push(sprite_data);
                    }
                    _ => ()
                }
            }
            _ => ()
        }
    }
}

pub mod debug {
    use super::{OamAddr, SpriteData, SpritePipeline};

    pub struct SpritePipelineState {
        pub oam_addr: OamAddr,
        pub oam_entry: u8,
        pub secondary_oam_index: u8,
        pub secondary_oam: [u8;32],
        pub output_units: [Option<SpriteData>;8],
    }

    impl SpritePipelineState {
        pub(in crate::ppu) fn new(p: &SpritePipeline) -> Self {
            let mut output = [None;8];
            p.sprite_output_units.borrow()
                .iter()
                .enumerate()
                .for_each(|(idx, sprite_data)| output[idx] = Some(sprite_data.clone()));
            SpritePipelineState {
                oam_addr: p.oam_addr.get(),
                oam_entry: p.oam_entry.get(),
                secondary_oam_index: p.secondary_oam_index.get(),
                secondary_oam: p.secondary_oam.get(),
                output_units: output,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_oam_addr_incr_low() {
        let mut addr = OamAddr::default();
        assert!(!addr.incr_low());
        assert_eq!(1, addr.low());
        assert!(!addr.incr_low());
        assert_eq!(2, addr.low());
        assert!(!addr.incr_low());
        assert_eq!(3, addr.low());
        assert!(addr.incr_low());
        assert_eq!(0, addr.low())
    }

    #[test]
    fn test_oam_addr_incr_high_wraps() {
        let mut addr = OamAddr::default();
        assert_eq!(0, addr.high());
        for i in 0..63 {
            assert!(!addr.incr_high());
            assert_eq!(i+1, addr.high());
        }
        assert!(addr.incr_high());
        assert_eq!(0, addr.high())
    }

    #[test]
    fn test_sprite_pipeline_clear() {
        let pipeline = SpritePipeline::default();

        let mut oam = [0u8; 32];
        assert_eq!(oam, pipeline.secondary_oam.get());

        pipeline.clear(0);

        oam[0] = 0xFF;
        assert_eq!(oam, pipeline.secondary_oam.get());

        pipeline.clear(2);
        oam[2] = 0xFF;
        assert_eq!(oam, pipeline.secondary_oam.get());
    }

    #[test]
    fn test_sprite_pipeline_write() {
        let pipeline = SpritePipeline::default();

        let mut oam = [0u8; 32];
        assert_eq!(oam, pipeline.secondary_oam.get());

        pipeline.write_u8(0xEE);
        oam[0] = 0xEE;
        assert_eq!(oam, pipeline.secondary_oam.get());

        pipeline.secondary_oam_index.set(24);
        pipeline.write_u8(0xAB);
        oam[24] = 0xAB;
        assert_eq!(oam, pipeline.secondary_oam.get());
    }

    #[test]
    fn test_sprite_pipeline_is_full() {
        let pipeline = SpritePipeline::default();
        assert!(!pipeline.is_full());
        pipeline.secondary_oam_index.set(31);
        assert!(!pipeline.is_full());
        pipeline.secondary_oam_index.set(32);
        assert!(pipeline.is_full());
        pipeline.secondary_oam_index.set(64);
        assert!(pipeline.is_full());
    }

    #[test]
    fn test_sprite_pipeline_sprite_count() {
        let pipeline = SpritePipeline::default();
        assert_eq!(0, pipeline.sprite_count());
        pipeline.secondary_oam_index.set(1);
        assert_eq!(0, pipeline.sprite_count());
        pipeline.secondary_oam_index.set(2);
        assert_eq!(0, pipeline.sprite_count());
        pipeline.secondary_oam_index.set(3);
        assert_eq!(0, pipeline.sprite_count());

        pipeline.secondary_oam_index.set(4);
        assert_eq!(1, pipeline.sprite_count());
        pipeline.secondary_oam_index.set(32);
        assert_eq!(8, pipeline.sprite_count());
    }
}
