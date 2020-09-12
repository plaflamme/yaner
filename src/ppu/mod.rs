#![allow(non_upper_case_globals)]

use crate::cartridge::Mapper;
use crate::memory::Ram256;
use crate::memory::{AddressSpace, Mirrored, Ram2KB, Ram32};
use bitflags::bitflags;
use rand::{thread_rng, Rng};
use std::cell::{Cell, RefCell};
use std::ops::Generator;
use std::rc::Rc;
use bitregions::bitregions;
use std::ops::{BitAnd, BitOr};
pub mod debug;

bitflags! {
    // http://wiki.nesdev.com/w/index.php/PPU_programmer_reference#PPUCTRL
    pub struct PpuCtrl: u8 {
        const N_LO = 1 << 0;
        const N_HI = 1 << 1;
        const N = Self::N_LO.bits | Self::N_HI.bits; // Base nametable address (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)

        const I = 1 << 2; // VRAM address increment

        const S = 1 << 3; // Sprite pattern table address


        const B = 1 << 4; // Background pattern table
        const H = 1 << 5; // Sprite size
        const P = 1 << 6; // PPU master/slave select
        const V = 1 << 7; // Generate an NMI at the start of the vertical blanking interval
    }
}

impl Default for PpuCtrl {
    fn default() -> Self {
        Self::empty()
    }
}

impl PpuCtrl {
    fn vram_inc_step(&self) -> u16 {
        // (0: add 1, going across; 1: add 32, going down)
        if self.contains(PpuCtrl::I) {
            32
        } else {
            1
        }
    }

    fn bg_pattern_table_address(&self) -> u16 {
        // (0: $0000; 1: $1000)
        if self.contains(PpuCtrl::B) {
            0x1000
        } else {
            0x0000
        }
    }
}

bitflags! {
    // http://wiki.nesdev.com/w/index.php/PPU_programmer_reference#PPUMASK
    pub struct PpuMask: u8 {
        const GREYSCALE = 1 << 0; // Greyscale
        const m = 1 << 1; // 1: Show background in leftmost 8 pixels of screen, 0: Hide
        const M = 1 << 2; // 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
        const b = 1 << 3; // Show background

        const s = 1 << 4; // Show sprites
        const R = 1 << 5; // Emphasize red
        const G = 1 << 6; // Emphasize green
        const B = 1 << 7; // Emphasize blue
    }
}

impl Default for PpuMask {
    fn default() -> Self {
        Self::empty()
    }
}

bitflags! {
    // http://wiki.nesdev.com/w/index.php/PPU_programmer_reference#PPUSTATUS
    pub struct PpuStatus: u8 {
        const O = 1 << 5; // Sprite overflow
        const S = 1 << 6; // Sprite 0 Hit
        const V = 1 << 7; // vblank
    }
}

// http://wiki.nesdev.com/w/index.php/PPU_power_up_state
impl Default for PpuStatus {
    fn default() -> Self {
        let mut status = Self::empty();
        // V and O are "often set" on power up
        status.insert(PpuStatus::V);
        status.insert(PpuStatus::O);
        // S is always set to 0
        status.remove(PpuStatus::S);
        status
    }
}

#[derive(Clone, Default)]
pub struct RegisterPair<T: Copy> {
    pub low: Cell<T>,
    pub high: Cell<T>,
}

#[derive(Clone, Default)]
pub struct LatchedPair<T: Copy> {
    pub latch: RegisterPair<u8>,
    pub value: RegisterPair<T>
}

impl LatchedPair<u16> {
    fn latch(&self) {
        self.value.low.update(|v| (v & 0xFF00) | self.latch.low.get() as u16);
        self.value.high.update(|v| (v & 0xFF00) | self.latch.high.get() as u16);
    }
    fn shift(&self) {
        self.value.low.update(|v| v << 1);
        self.value.high.update(|v| v << 1);
    }
}

bitregions! {
    pub VramAddress u16 {
        FINE_Y: 0b111_00_00000_00000, // Fine y scroll
        NAMETABLE: 0b000_11_00000_00000, // Nametable select
        COARSE_Y: 0b000_00_11111_00000, // Coarse y scroll
        COARSE_X: 0b000_00_00000_11111, // Coarse x scroll
    }
}

impl VramAddress {
    fn increment(&mut self, step: u16) {
        self.0 = self.0.wrapping_add(step);
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Tile_and_attribute_fetching
    // this value as an address into the nametable
    fn nametable_addr(&self) -> u16 {
        // TODO: don't we need to factor in base address in PPUCTRL?
        0x2000 | (self.0 & 0x0FFF)
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Tile_and_attribute_fetching
    // this value as an address into the attribute table
    fn attribute_addr(&self) -> u16 {
        let v: u16 = self.0.into();
        0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Coarse_X_increment
    fn incr_x(&mut self) {
        if self.coarse_x() == 31 {
            self.set_coarse_x(0u16);
            self.0 ^= 0x0400;
        } else {
            self.set_coarse_x(self.coarse_x() + 1);
        }
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Y_increment
    fn incr_y(&mut self) {
        let fy = self.fine_y();
        if fy < 7 {
            self.set_fine_y(fy + 1);
        } else {
            self.set_fine_y(0u16);
            let cy = self.coarse_y();
            if cy == 29 {
                self.set_coarse_y(0u16);
                self.0 ^= 0x0800;
            } else if cy == 31 {
                self.set_coarse_y(0u16);
            } else {
                self.set_coarse_y(cy + 1);
            }
        }
    }
}

pub struct Ppu {
    pub bus: PpuBus,

    ctrl: Cell<PpuCtrl>,
    mask: Cell<PpuMask>,
    status: Cell<PpuStatus>,

    oam_addr: Cell<u8>, // OAMADDR

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#PPU_internal_registers
    // Accessed through PPUSCROLL and PPUADDR
    t_addr: Cell<VramAddress>,
    v_addr: Cell<VramAddress>,
    addr_latch: Cell<bool>, // this is referred to as w in the wiki
    fine_x: Cell<u8>,

    // internal OAM memory, enough for 64 sprites of 4 bytes each
    oam_data: Ram256,

    // http://wiki.nesdev.com/w/index.php/PPU_registers#Ports
    open_bus: Cell<u8>,

    // Rendering state
    scanline: Cell<u16>,
    dot: Cell<u16>,
    // 2 16-bit shift registers. These contain the pattern table data for two tiles [...]
    pattern_data: LatchedPair<u16>,
    // 2 8-bit shift registers. These contain the palette attributes for the lower 8 pixels of the 16-bit shift register [...]
    attribute_data: LatchedPair<u8>,

    frame_pixels: Cell<[u8; 256 * 240]>,

    // Temporary storage for tile fetching pipeline
    fetch_addr: Cell<u16>,
    nametable_entry: Cell<u8>,
    attribute_entry: Cell<u8>,

    // https://wiki.nesdev.com/w/index.php/PPU_frame_timing#VBL_Flag_Timing
    suppress_vbl: Cell<bool>,
    suppress_nmi: Cell<bool>,
}

impl Ppu {
    pub fn new(mapper: Rc<RefCell<Box<dyn Mapper>>>) -> Self {
        Ppu {
            bus: PpuBus::new(mapper),

            ctrl: Cell::new(PpuCtrl::default()),
            mask: Cell::new(PpuMask::default()),
            status: Cell::new(PpuStatus::default()),
            oam_addr: Cell::new(0x00),

            t_addr: Cell::new(VramAddress::default()),
            v_addr: Cell::new(VramAddress::default()),
            addr_latch: Cell::new(false),
            fine_x: Cell::new(0),

            oam_data: Ram256::new(),

            open_bus: Cell::new(0),

            scanline: Cell::new(0),
            dot: Cell::new(0),
            pattern_data: LatchedPair::default(),
            attribute_data: LatchedPair::default(),
            frame_pixels: Cell::new([0u8; 256 * 240]),

            fetch_addr: Cell::new(0),
            nametable_entry: Cell::new(0),
            attribute_entry: Cell::new(0),

            suppress_vbl: Cell::new(false),
            suppress_nmi: Cell::new(false),
        }
    }

    fn status(&self) -> u8 {
        let mut status = self.status.get();
        self.status.update(|s| s - PpuStatus::V);

        // reading PPUSTATUS resets the address latch
        self.addr_latch.set(false);

        // interactions between vbl and PPUSTATUS reads:
        //   Reading PPUSTATUS one PPU clock before reads it as clear and never sets the flag
        //     or generates NMI for that frame
        //   Reading PPUSTATUS on the same PPU clock or one later reads it as set, clears it,
        //     and suppresses the NMI for that frame
        match (self.scanline.get(), self.dot.get()) {
            (241, 0) => {
                self.suppress_vbl.set(true);
                self.suppress_nmi.set(true);
            }
            (241, 1) => {
                // the ppu would have set it on this clock tick
                status = status | PpuStatus::V;
                self.suppress_vbl.set(true); // so we don't set it
                self.suppress_nmi.set(true);
            }
            (241, 2..=3) => self.suppress_nmi.set(true),
            (261, 1) => {
                // the ppu will clear it on this tick
                status = status - PpuStatus::V;
            }
            _ => (),
        }

        status.bits()
    }

    fn write_scroll(&self, value: u8) {
        let latch = self.addr_latch.get();
        if latch {
            self.t_addr.update(|mut t| {
                t.set_fine_y(value & 0b0000_0111);
                t.set_coarse_y(value >> 3);
                t
            });
        } else {
            self.fine_x.set(value & 0b0000_0111);
            self.t_addr.update(|mut t| {
                t.set_coarse_x(value >> 3);
                t
            });
        }
        self.addr_latch.set(!latch);
    }

    fn write_addr(&self, value: u8) {
        let u16_value = value as u16;
        let latch = self.addr_latch.get();
        if latch {
            self.t_addr.update(|v| (v & 0xFF00u16) | u16_value);
            // transfers to the v register
            self.v_addr.set(self.t_addr.get());
        } else {
            self.t_addr.update(|v| (v & 0x00FFu16) | (u16_value << 8));
        }
        self.addr_latch.set(!latch);
    }

    fn vram_read_u8(&self) -> u8 {
        let addr: u16 = self.v_addr.get().into();
        let data = self.bus.read_u8(addr);
        let step = self.ctrl.get().vram_inc_step();
        self.v_addr.update(|mut v| {
            v.increment(step);
            v
        });
        data
    }

    fn vram_write_u8(&self, value: u8) {
        let addr: u16 = self.v_addr.get().into();
        self.bus.write_u8(addr, value);
        let step = self.ctrl.get().vram_inc_step();
        self.v_addr.update(|mut v| {
            v.increment(step);
            v
        });
    }

    pub fn decay_open_bus(&self) {
        let mut rng = thread_rng();
        for i in 0..8 {
            if rng.gen_bool(0.25) {
                self.open_bus.set(self.open_bus.get() & !(1 << i));
            }
        }
    }

    fn evaluate_sprites(&self, pre_render: bool) {
        match self.dot.get() {
            1 => {
                // TODO: clear secondary OAM
                if pre_render {
                    // Clear sprite overflow and 0hit
                    self.status.update(|s| s - PpuStatus::S - PpuStatus::O);
                }
            }
            256 => (),// TODO: sprite evaluation is done (for next scanline) at this point
            320 => (),// TODO: sprite tile fetching is done (for next scanline) at this point
            _ => (),
        }
    }

    fn render_pixel(&self) {
        match self.dot.get() {
            2..=257 | 322..=337 => {
                // NOTE: on the second tick, we draw pixel 0
                // TODO: the wiki says "Actual pixel output is delayed further due to internal render pipelining, and the first pixel is output during cycle 4."
                let pixel = self.dot.get() - 2;
                let bg_color = self.render_background_pixel(pixel);
                // let (sprite_color, sprite_behind) = self.render_sprite_pixel(pixel);
                //
                // let colors = if sprite_behind {
                //     [bg_color, sprite_color]
                // } else {
                //     [sprite_color, bg_color]
                // };

                // let pixel_color = if colors[0] == 0 { colors[1] } else { colors[0] };
                if self.scanline.get() < 241 && self.dot.get() < 257 {
                    let pixel_index = pixel + self.scanline.get() * 256;

                    let s: &Cell<[u8]> = &self.frame_pixels;
                    let pixels = s.as_slice_of_cells();
                    pixels[pixel_index as usize].set(bg_color);
                }

                self.pattern_data.shift();
            }
            _ => (),
        }
    }

    fn render_sprite_pixel(&self, _dot: u16) -> (u8, bool) {
        if !self.mask.get().contains(PpuMask::s) {
            (0, false)
        } else {
            // TODO
            (0, false)
        }
    }

    fn render_background_pixel(&self, _dot: u16) -> u8 {
        if !self.mask.get().contains(PpuMask::b) {
            0
        } else {
            // TODO: scrolling affects this
            let high = (self.pattern_data.value.high.get() >> 14) & 0b0000_0010;
            let low = (self.pattern_data.value.low.get() >> 15) & 0x01;
            (high | low) as u8
        }
    }

    fn fetch_tile(&self, pre_render: bool) {
        match self.dot.get() {
            1..=256 | 321..=337 => {
                match self.dot.get() % 8 {
                    1 => {
                        self.fetch_addr.set(self.v_addr.get().nametable_addr());

                        if self.dot.get() > 1 && self.dot.get() < 321 {
                            self.pattern_data.latch();
                            // TODO: not sure what we're supposed to do for attribute_data
                        }
                    },
                    2 => {
                        let entry = self.bus.vram.read_u8(self.fetch_addr.get());
                        self.nametable_entry.set(entry);
                    },
                    3 => {
                        self.fetch_addr.set(self.v_addr.get().attribute_addr());
                    },
                    4 => {
                        let entry = self.bus.vram.read_u8(self.fetch_addr.get());
                        // TODO: Scrolling affects this.
                        self.attribute_entry.set(entry);
                    },
                    5 => {
                        // TODO: Scrolling affects this.
                        let index = self.nametable_entry.get() as u16 * 16 | (self.v_addr.get().fine_y() as u16);
                        self.fetch_addr.set(index + self.ctrl.get().bg_pattern_table_address());
                    },
                    6 => {
                        let pattern = self.bus.read_u8(self.fetch_addr.get());
                        self.pattern_data.latch.low.set(pattern);
                    },
                    7 =>  {
                        self.fetch_addr.update(|v| v + 8);
                    }
                    0 => {
                        let pattern = self.bus.read_u8(self.fetch_addr.get());
                        self.pattern_data.latch.high.set(pattern);
                        if self.mask.get().contains(PpuMask::b) { // TODO also for sprite rendering
                            self.v_addr.update(|mut v| {
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
                // TODO: scrolling, copy t into x
            }
            280..=304 => if pre_render {
                // TODO: scrolling, copy t into y
            },
            338 => {
                let entry = self.bus.vram.read_u8(self.fetch_addr.get());
                self.nametable_entry.set(entry);
            }
            339 => {
                self.fetch_addr.set(self.v_addr.get().nametable_addr());
            }
            340 => {
                let entry = self.bus.vram.read_u8(self.fetch_addr.get());
                self.nametable_entry.set(entry);
            }

            _ => (),
        }
    }

    pub fn run<'a>(&'a self) -> impl Generator<Yield = PpuCycle, Return = ()> + 'a {
        let mut generate_nmi = false;
        move || loop {
            match (self.scanline.get(), self.dot.get()) {
                (0..=239, _) => {
                    self.evaluate_sprites(false);
                    self.fetch_tile(false);
                    self.render_pixel();
                }
                (241, 1) => {
                    if !self.suppress_vbl.get() {
                        // enable vblank
                        self.status.update(|s| s | PpuStatus::V);

                        if !self.suppress_nmi.get() && self.ctrl.get().contains(PpuCtrl::V) {
                            generate_nmi = true;
                        }
                    }
                }
                (261, _) => {
                    if self.dot.get() == 1 {
                        self.status.update(|s| s - PpuStatus::V);
                    }
                    self.evaluate_sprites(true);
                    self.fetch_tile(true);
                    self.render_pixel();
                }

                _ => (),
            }

            self.dot.update(|dot| (dot + 1) % 341);
            if self.dot.get() == 0 {
                self.scanline.update(|sc| (sc + 1) % 262);
            }

            let previous_ctrl = self.ctrl.get();

            if self.scanline.get() == 0 && self.dot.get() == 0 {
                self.suppress_vbl.set(false);
                self.suppress_nmi.set(false);
                yield PpuCycle::Frame
            } else {
                if !self.suppress_nmi.get()
                    && generate_nmi
                    && self.status.get().contains(PpuStatus::V)
                {
                    yield PpuCycle::Nmi
                } else {
                    yield PpuCycle::Tick
                }
            }
            if self.status.get().contains(PpuStatus::V) {
                // generate an nmi if PpuCtrl::V was enabled in the last cpu cycle.
                generate_nmi = !self.suppress_nmi.get()
                    && self.ctrl.get().contains(PpuCtrl::V)
                    && !previous_ctrl.contains(PpuCtrl::V);
            }
        }
    }
}

#[derive(Debug)]
pub enum PpuCycle {
    Tick,
    Nmi, // same as tick, but nmi should be triggered in the cpu
    Frame,
}

pub struct PpuBus {
    pub vram: Mirrored<Ram2KB>,
    palette: Mirrored<Ram32>,
    mapper: Rc<RefCell<Box<dyn Mapper>>>,
}

impl PpuBus {
    pub fn new(mapper: Rc<RefCell<Box<dyn Mapper>>>) -> Self {
        PpuBus {
            vram: Mirrored::new(Ram2KB::new(), 0x800, 0x2000),
            palette: Mirrored::new(Ram32::new(), 0x20, 0x3F00),
            mapper,
        }
    }
}

impl AddressSpace for PpuBus {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.mapper.borrow().ppu_addr_space().read_u8(addr),
            0x2000..=0x3EFF => self.vram.read_u8(addr),
            0x3F00..=0x3FFF => self.palette.read_u8(addr),
            _ => invalid_address!(addr),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => self.mapper.borrow().ppu_addr_space().write_u8(addr, value),
            0x2000..=0x3EFF => self.vram.write_u8(addr, value),
            0x3F00..=0x3FFF => self.palette.write_u8(addr, value),
            _ => invalid_address!(addr),
        }
    }
}

pub struct MemoryMappedRegisters {
    ppu: Rc<Ppu>,
}

impl MemoryMappedRegisters {
    pub fn new(ppu: Rc<Ppu>) -> Self {
        MemoryMappedRegisters { ppu }
    }
}

impl AddressSpace for MemoryMappedRegisters {
    fn read_u8(&self, addr: u16) -> u8 {
        let result = match addr {
            0x2000 => self.ppu.open_bus.get(),
            0x2001 => self.ppu.open_bus.get(),
            0x2002 => self.ppu.status() | (self.ppu.open_bus.get() & 0b0001_1111),
            0x2003 => self.ppu.open_bus.get(),
            0x2004 => {
                // http://wiki.nesdev.com/w/index.php/PPU_OAM#Byte_2
                // bits 2-4 of byte 2 are "unimplemented" and thus, should be cleared
                let addr = self.ppu.oam_addr.get() as u16;
                let mask = if addr % 4 == 2 { 0b1110_0011 } else { 0xFF };
                self.ppu.oam_data.read_u8(addr) & mask
            }
            0x2005 => self.ppu.open_bus.get(),
            0x2006 => self.ppu.open_bus.get(),
            0x2007 => {
                let bus_mask = match self.ppu.v_addr.get().into() {
                    0x3F00..=0x3FFF => self.ppu.open_bus.get() & 0b1100_0000, // palette values are 6bits wide
                    _ => 0x00,
                };
                self.ppu.vram_read_u8() | bus_mask
            }
            _ => invalid_address!(addr),
        };
        self.ppu.open_bus.set(result);
        result
    }

    fn write_u8(&self, addr: u16, value: u8) {
        self.ppu.open_bus.set(value);
        match addr {
            0x2000 => {
                self.ppu.ctrl.set(PpuCtrl::from_bits_truncate(value));
                // writing to the control register also sets the resulting nametable bits in t_addr
                self.ppu.t_addr.update(|mut t| {
                    t.set_nametable(value & 0b0000_0011);
                    t
                });
            },
            0x2001 => self.ppu.mask.set(PpuMask::from_bits_truncate(value)),
            0x2002 => (),
            0x2003 => self.ppu.oam_addr.set(value),
            0x2004 => {
                self.ppu
                    .oam_data
                    .write_u8(self.ppu.oam_addr.get() as u16, value);
                let addr = self.ppu.oam_addr.get();
                self.ppu.oam_addr.set(addr.wrapping_add(1));
            }
            0x2005 => self.ppu.write_scroll(value),
            0x2006 => self.ppu.write_addr(value),
            0x2007 => self.ppu.vram_write_u8(value),
            _ => invalid_address!(addr),
        }
    }
}
