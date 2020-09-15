use std::cell::Cell;
use std::ops::Generator;
use bitregions::bitregions;

use super::rgb;
use crate::memory::AddressSpace;
use crate::ppu::reg::{PpuStatus, PpuMask, PpuCtrl};
use crate::ppu::{PpuCycle, Registers};

#[derive(Clone, Default)]
pub struct RegisterPair<T: Copy> {
    pub low: Cell<T>,
    pub high: Cell<T>,
}

#[derive(Clone, Default)]
pub struct PatternData {
    pub latch: RegisterPair<u8>,
    pub value: RegisterPair<u16>
}

impl PatternData {
    pub fn latch(&self) {
        self.value.low.update(|v| (v & 0xFF00) | self.latch.low.get() as u16);
        self.value.high.update(|v| (v & 0xFF00) | self.latch.high.get() as u16);
    }

    pub fn shift(&self) {
        self.value.low.update(|v| v << 1);
        self.value.high.update(|v| v << 1);
    }
}

#[derive(Clone, Default)]
pub struct AttributeData {
    pub latch: RegisterPair<u8>, // this is actually a 1bit latch
    pub value: RegisterPair<u8>
}

impl AttributeData {

    pub fn latch(&self) {
        self.value.low.update(|v| (v & 0xFE) | self.latch.low.get() & 1);
        self.value.high.update(|v| (v & 0xFE) | self.latch.high.get() & 1);
    }

    pub fn shift(&self) {
        self.value.low.update(|v| v << 1 | self.latch.low.get());
        self.value.high.update(|v| v << 1 | self.latch.high.get());
    }
}

bitregions! {
    pub PaletteColor u8 {
        COLOR: 0b0000_0011,
        PALETTE: 0b0000_1100,
    }
}

impl PaletteColor {

    fn from(palette: u8, color: u8) -> Self {
        let mut p = PaletteColor::default();
        p.set_color(color);
        p.set_palette(palette);
        p
    }

    fn address(&self) -> u16 {
        0x3F00 | (self.palette() as u16) << 2 | self.color() as u16
    }

    fn is_transparent(&self) -> bool {
        self.color() == 0
    }
}

#[derive(Clone, Copy, Default)]
pub struct Pixel(u8);

impl Pixel {
    pub fn rgb(&self) -> rgb::Rgb {
        rgb::SYSTEM_PALETTE[self.0 as usize]
    }
}

pub struct Renderer {
    // the scanline and dot we're about to draw
    pub scanline: Cell<u16>,
    pub dot: Cell<u16>,
    // 2 16-bit shift registers. These contain the pattern table data for two tiles [...]
    pub(super) pattern_data: PatternData,
    // 2 8-bit shift registers. These contain the palette attributes for the lower 8 pixels of the 16-bit shift register [...]
    pub(super) attribute_data: AttributeData,

    // Temporary storage for tile fetching pipeline
    pub(super) fetch_addr: Cell<u16>,
    pub(super) nametable_entry: Cell<u8>,

    pub(super) frame_pixels: Cell<[Pixel; 256 * 240]>,

    // https://wiki.nesdev.com/w/index.php/PPU_frame_timing#VBL_Flag_Timing
    suppress_vbl: Cell<bool>,
    suppress_nmi: Cell<bool>,
}

impl Renderer {

    pub fn new() -> Self {
        Renderer {
            scanline: Cell::new(0),
            dot: Cell::new(0),
            pattern_data: PatternData::default(),
            attribute_data: AttributeData::default(),
            frame_pixels: Cell::new([Pixel::default(); 256 * 240]),
            fetch_addr: Cell::new(0),
            nametable_entry: Cell::new(0),
            suppress_vbl: Cell::new(false),
            suppress_nmi: Cell::new(false),
        }
    }

    // interactions between vbl and PPUSTATUS reads:
    //   Reading PPUSTATUS one PPU clock before reads it as clear and never sets the flag
    //     or generates NMI for that frame
    //   Reading PPUSTATUS on the same PPU clock or one later reads it as set, clears it,
    //     and suppresses the NMI for that frame
    pub(super) fn ppustatus_read(&self, status: &mut PpuStatus) {
        match (self.scanline.get(), self.dot.get()) {
            (241, 0) => {
                self.suppress_vbl.set(true);
                self.suppress_nmi.set(true);
            }
            (241, 1) => {
                // the ppu would have set it on this clock tick
                status.insert(PpuStatus::V);
                self.suppress_vbl.set(true); // so we don't set it
                self.suppress_nmi.set(true);
            }
            (241, 2..=3) => self.suppress_nmi.set(true),
            // the ppu will clear it on this tick
            (261, 1) => status.remove(PpuStatus::V),
            _ => (),
        };
    }

    fn evaluate_sprites(&self, registers: &Registers, pre_render: bool) {
        match self.dot.get() {
            1 => {
                // TODO: clear secondary OAM
                if pre_render {
                    // Clear sprite overflow and 0hit
                    registers.status.update(|s| s - PpuStatus::S - PpuStatus::O);
                }
            }
            256 => (),// TODO: sprite evaluation is done (for next scanline) at this point
            320 => (),// TODO: sprite tile fetching is done (for next scanline) at this point
            _ => (),
        }
    }

    fn render_pixel(&self, registers: &Registers, bus: &dyn AddressSpace) {
        match self.dot.get() {
            // From http://wiki.nesdev.com/w/images/4/4f/Ppu.svg
            //   The background shift registers shift during each of dots 2...257 and 322...337, inclusive.
            2..=257 | 322..=337 => {
                // NOTE: on the second tick, we draw pixel 0
                // TODO: the wiki says "Actual pixel output is delayed further due to internal render pipelining, and the first pixel is output during cycle 4."
                let pixel = self.dot.get() - 2;
                let bg_color = self.render_background_pixel(registers, pixel);
                let (sprite_color, sprite_behind) = self.render_sprite_pixel(registers, pixel);

                let colors = if sprite_behind {
                    [bg_color, sprite_color]
                } else {
                    [sprite_color, bg_color]
                };

                let pixel_color = if colors[0].is_transparent() { colors[1] } else { colors[0] };
                if self.scanline.get() < 241 && self.dot.get() < 257 {
                    let pixel_index = pixel + self.scanline.get() * 256;

                    let s: &Cell<[Pixel]> = &self.frame_pixels;
                    let pixels = s.as_slice_of_cells();
                    let pixel_value = Pixel(bus.read_u8(pixel_color.address()));
                    pixels[pixel_index as usize].set(pixel_value);
                }

                self.pattern_data.shift();
                self.attribute_data.shift();
            }
            _ => (),
        }
    }

    fn render_sprite_pixel(&self, registers: &Registers, _dot: u16) -> (PaletteColor, bool) {
        if !registers.mask.get().contains(PpuMask::s) {
            (PaletteColor::default(), false)
        } else {
            // TODO
            (PaletteColor::default(), false)
        }
    }

    fn render_background_pixel(&self, registers: &Registers, _dot: u16) -> PaletteColor {
        if !registers.mask.get().contains(PpuMask::b) {
            PaletteColor::default()
        } else {

            // From http://wiki.nesdev.com/w/index.php/PPU_rendering
            //   Every cycle, a bit is fetched from the 4 background shift registers in order to create a pixel on screen.
            //   Exactly which bit is fetched depends on the fine X scroll, set by $2005 (this is how fine X scrolling is possible).
            //   Afterwards, the shift registers are shifted once, to the data for the next pixel.
            // The last step is implemented in self.shift()

            // TODO: scrolling affects this
            let high = (self.pattern_data.value.high.get() >> 14) & 0b10;
            let low = (self.pattern_data.value.low.get() >> 15) & 0x01;

            let color = (high | low) as u8;

            let high = (self.attribute_data.value.high.get() >> (6 - registers.fine_x.get())) & 0b10 ;
            let low = (self.attribute_data.value.low.get() >> (7 - registers.fine_x.get())) & 0x01;

            let palette = (high | low) as u8;

            PaletteColor::from(palette, color)
        }
    }

    fn fetch_tile(&self, registers: &Registers, bus: &dyn AddressSpace, pre_render: bool) {
        match self.dot.get() {
            1..=256 | 321..=337 => {
                match self.dot.get() % 8 {
                    1 => {
                        self.fetch_addr.set(registers.v_addr.get().nametable_addr());

                        // The shifters are reloaded during ticks 9, 17, 25, ..., 257 and ticks 329 and 337.
                        if self.dot.get() > 1 && self.dot.get() < 321 {
                            self.pattern_data.latch();
                            self.attribute_data.latch();
                        }
                    },
                    2 => {
                        let entry = bus.read_u8(self.fetch_addr.get());
                        self.nametable_entry.set(entry);
                    },
                    3 => {
                        self.fetch_addr.set(registers.v_addr.get().attribute_addr());
                    },
                    4 => {
                        let mut entry = bus.read_u8(self.fetch_addr.get());
                        if registers.v_addr.get().coarse_y() & 2 != 0 {
                            entry >>= 4;
                        }
                        if registers.v_addr.get().coarse_x() & 2 != 0 {
                            entry >>= 2;
                        }
                        self.attribute_data.latch.low.set(entry & 1);
                        self.attribute_data.latch.high.set((entry & 2) >> 1);
                    },
                    5 => {
                        // TODO: Scrolling affects this.
                        let index = self.nametable_entry.get() as u16 * 16 | (registers.v_addr.get().fine_y() as u16);
                        self.fetch_addr.set(index + registers.ctrl.get().bg_pattern_table_address());
                    },
                    6 => {
                        let pattern = bus.read_u8(self.fetch_addr.get());
                        self.pattern_data.latch.low.set(pattern);
                    },
                    7 =>  {
                        self.fetch_addr.update(|v| v + 8);
                    }
                    0 => {
                        let pattern = bus.read_u8(self.fetch_addr.get());
                        self.pattern_data.latch.high.set(pattern);
                        if registers.mask.get().is_rendering() {
                            registers.v_addr.update(|mut v| {
                                if self.dot.get() == 256 {
                                    v.incr_y();
                                } else {
                                    v.incr_x();
                                }
                                v
                            });
                        }
                    },
                    _ => unreachable!("match on % 8 is exhaustive")
                }
            }
            257 => {
                self.pattern_data.latch();
                self.attribute_data.latch();
                // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#At_dot_257_of_each_scanline
                if registers.mask.get().is_rendering() {
                    registers.v_addr.update(|mut v| {
                        v.copy_horizontal_bits(&registers.t_addr.get());
                        v
                    });
                }
            }
            280..=304 => if pre_render {
                // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#During_dots_280_to_304_of_the_pre-render_scanline_.28end_of_vblank.29
                if registers.mask.get().is_rendering() {
                    registers.v_addr.update(|mut v| {
                        v.copy_vertical_bits(&registers.t_addr.get());
                        v
                    });
                }
            },
            338 => {
                let entry = bus.read_u8(self.fetch_addr.get());
                self.nametable_entry.set(entry);
            }
            339 => {
                self.fetch_addr.set(registers.v_addr.get().nametable_addr());
            }
            340 => {
                let entry = bus.read_u8(self.fetch_addr.get());
                self.nametable_entry.set(entry);
            }

            _ => (),
        }
    }

    pub(super) fn run<'a>(&'a self, registers: &'a Registers, bus: &'a dyn AddressSpace) -> impl Generator<Yield = PpuCycle, Return = ()> + 'a {
        let mut generate_nmi = false;
        let mut odd_frame = false;
        move || loop {
            match (self.scanline.get(), self.dot.get()) {
                (0..=239, _) => {
                    self.evaluate_sprites(registers, false);
                    self.render_pixel(registers, bus);
                    self.fetch_tile(registers, bus, false);
                }
                (241, 1) => {
                    if !self.suppress_vbl.get() {
                        // enable vblank
                        registers.status.update(|s| s | PpuStatus::V);

                        if !self.suppress_nmi.get() && registers.ctrl.get().contains(PpuCtrl::V) {
                            generate_nmi = true;
                        }
                    }
                }
                (261, _) => {
                    if self.dot.get() == 1 {
                        registers.status.update(|s| s - PpuStatus::V);
                    }
                    self.evaluate_sprites(registers, true);
                    self.render_pixel(registers, bus);
                    self.fetch_tile(registers, bus, true);

                    if odd_frame && registers.mask.get().is_rendering() && self.dot.get() == 339 {
                        self.dot.update(|dot| dot + 1); // even/odd frame, skip to 0,0
                    }
                }

                _ => (),
            }

            self.dot.update(|dot| (dot + 1) % 341);
            if self.dot.get() == 0 {
                self.scanline.update(|sc| (sc + 1) % 262);
            }

            let previous_ctrl = registers.ctrl.get();

            if self.scanline.get() == 0 && self.dot.get() == 0 {
                self.suppress_vbl.set(false);
                self.suppress_nmi.set(false);
                yield PpuCycle::Frame;
                self.frame_pixels.set([Pixel::default(); 256 * 240]);
                odd_frame = !odd_frame;
            } else {
                if !self.suppress_nmi.get()
                    && generate_nmi
                    && registers.status.get().contains(PpuStatus::V)
                {
                    yield PpuCycle::Nmi
                } else {
                    yield PpuCycle::Tick
                }
            }
            if registers.status.get().contains(PpuStatus::V) {
                // generate an nmi if PpuCtrl::V was enabled in the last cpu cycle.
                generate_nmi = !self.suppress_nmi.get()
                    && registers.ctrl.get().contains(PpuCtrl::V)
                    && !previous_ctrl.contains(PpuCtrl::V);
            }
        }
    }

}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pattern_data_latch() {
        let pd = PatternData::default();
        pd.latch();
        assert_eq!(pd.value.low.get(), 0);
        assert_eq!(pd.value.high.get(), 0);

        pd.latch.low.set(0xAA);
        pd.latch.high.set(0xBB);
        pd.latch();
        assert_eq!(pd.value.low.get(), 0x00AA);
        assert_eq!(pd.value.high.get(), 0x00BB);

        pd.value.low.set(0x1100);
        pd.value.high.set(0x2200);
        pd.latch();
        assert_eq!(pd.value.low.get(), 0x11AA);
        assert_eq!(pd.value.high.get(), 0x22BB);
    }

    #[test]
    fn test_pattern_data_shift() {
        let pd = PatternData::default();
        pd.shift();
        assert_eq!(pd.value.low.get(), 0);
        assert_eq!(pd.value.high.get(), 0);

        pd.value.low.set(0b0101_0101_0101_0101);
        pd.value.high.set(0b1010_1010_1010_1010);
        pd.shift();
        assert_eq!(pd.value.low.get(), 0b1010_1010_1010_1010);
        assert_eq!(pd.value.high.get(), 0b0101_0101_0101_0100);
        pd.shift();
        assert_eq!(pd.value.low.get(), 0b0101_0101_0101_0100);
        assert_eq!(pd.value.high.get(), 0b1010_1010_1010_1000);
    }

    #[test]
    fn test_attribute_data_latch() {
        let ad = AttributeData::default();
        ad.latch();
        assert_eq!(ad.value.low.get(), 0);
        assert_eq!(ad.value.high.get(), 0);

        ad.latch.low.set(0x01);
        ad.latch.high.set(0x01);
        ad.latch();
        assert_eq!(ad.value.low.get(), 0x01);
        assert_eq!(ad.value.high.get(), 0x01);

        ad.value.low.set(0b1111_1110);
        ad.value.high.set(0b1111_1110);
        ad.latch();
        assert_eq!(ad.value.low.get(), 0b1111_1111);
        assert_eq!(ad.value.high.get(), 0b1111_1111);

        ad.value.low.set(0b0000_0000);
        ad.value.high.set(0b0000_0000);
        ad.latch();
        assert_eq!(ad.value.low.get(), 0b0000_0001);
        assert_eq!(ad.value.high.get(), 0b0000_0001);
    }

    #[test]
    fn test_attribute_data_shift() {
        let ad = AttributeData::default();
        ad.shift();
        assert_eq!(ad.value.low.get(), 0);
        assert_eq!(ad.value.high.get(), 0);

        ad.value.low.set(0b0101_0101);
        ad.value.high.set(0b1010_1010);
        ad.shift();
        assert_eq!(ad.value.low.get(), 0b1010_1010);
        assert_eq!(ad.value.high.get(), 0b0101_0100);
        ad.shift();
        assert_eq!(ad.value.low.get(), 0b0101_0100);
        assert_eq!(ad.value.high.get(), 0b1010_1000);
    }

}
