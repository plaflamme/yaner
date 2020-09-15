#![allow(non_upper_case_globals)]

use crate::cartridge::Mapper;
use crate::memory::Ram256;
use crate::memory::{AddressSpace, Mirrored, Ram2KB, Ram32};
use rand::{thread_rng, Rng};
use std::cell::{Cell, RefCell};
use std::ops::Generator;
use std::rc::Rc;

pub mod reg;
pub mod renderer;
pub mod vram_address;
pub mod debug;
pub mod rgb;

use reg::{PpuCtrl, PpuMask, PpuStatus};
use vram_address::VramAddress;
use renderer::Renderer;

// NOTES:
//   Nametable - this is stored in VRAM by the CPU. Each byte is an index into the pattern table.
//   Pattern table - these are 16 bytes values that contains the pattern that should be displayed on screen.
//     The 2 8-byte values are comined together to form 64 2-bit values (or 8x8 2-bit values)
//     The 2-bit values are indices into the color palette
//   Palettes - the NES has a static system palette that contains 64 colors. and each frame has its own palette which is a subset of the system palette.
//     Frame palette - the frame palette is 8 groups of 4 colours. Groups are indexed 0-7 and individual colors 0-3.
//       Palettes 0-3 are for the backgrounds, palettes 4-7 are for the sprites.
//       The frame palette is selected from the attribute table which is part of the nametable (thus also setup by the CPU, presumably during vblank)
//   Attribute table - the attribute table divides the frame into blocks of 4x4 tiles. Each block is also divided into 4 regions of 2x2 tiles.
//     Each byte in the attribute table represents a single block of 4x4 tiles.
//     Each byte is divided into 4 2-bit values, one per block region (2x2 tiles).
//       * top-left:  ------xx
//       * top-right: ----xx--
//       * bot-left:  --xx----
//       * bot-right: xx------

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

    renderer: Renderer,

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

            renderer: Renderer::new(),

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
        match (self.renderer.scanline.get(), self.renderer.dot.get()) {
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

    pub fn run<'a>(&'a self) -> impl Generator<Yield = PpuCycle, Return = ()> + 'a {
        self.renderer.run(&self)
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
