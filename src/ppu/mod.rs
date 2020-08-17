#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, RefCell};
use crate::memory::{AddressSpace, Ram2KB, Mirrored, Ram32};
use crate::memory::Ram256;
use std::ops::Generator;
use rand::{thread_rng, Rng};
use std::rc::Rc;
use bitflags::_core::marker::PhantomData;
use crate::cartridge::Mapper;

mod renderer;

bitflags! {
    // http://wiki.nesdev.com/w/index.php/PPU_programmer_reference#PPUCTRL
    struct PpuCtrl: u8 {
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
        if self.contains(PpuCtrl::I) { 32 } else { 0 }
    }
}

bitflags! {
    // http://wiki.nesdev.com/w/index.php/PPU_programmer_reference#PPUMASK
    struct PpuMask: u8 {
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
    struct PpuStatus: u8 {
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

pub struct Ppu {
    ctrl: Cell<PpuCtrl>,
    mask: Cell<PpuMask>,
    status: Cell<PpuStatus>,
    oam_addr: Cell<u8>,

    scroll_addr: Cell<u16>,
    data_addr: Cell<u16>,

    // shared by scroll and data
    addr_latch: Cell<bool>,

    oam_data: Ram256,

    // http://wiki.nesdev.com/w/index.php/PPU_registers#Ports
    open_bus: Cell<u8>,
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            ctrl: Cell::new(PpuCtrl::default()),
            mask: Cell::new(PpuMask::default()),
            status: Cell::new(PpuStatus::default()),
            oam_addr: Cell::new(0x00),

            scroll_addr: Cell::new(0),
            data_addr: Cell::new(0),

            addr_latch: Cell::new(false),

            oam_data: Ram256::new(),

            open_bus: Cell::new(0)
        }
    }

    fn status(&self) -> u8 {
        let status = self.status.get();
        let mut cleared = PpuStatus::from(status);
        cleared.remove(PpuStatus::V);
        self.status.set(cleared);

        // reading PPUSTATUS resets the address latch
        self.addr_latch.set(false);

        status.bits()
    }

    // This explains the way the addresses should be decoded into t, v and x.
    // http://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
    fn latched_write(&self, target: &Cell<u16>, value: u8) {
        let latch = self.addr_latch.get();
        let current = target.get();

        let addr = if latch {
            let addr_lo = value as u16;
            let addr_hi = current & 0xFF00;
            addr_hi | addr_lo
        } else {
            let addr_lo = current & 0x00FF;
            let addr_hi = (value as u16) << 8;
            addr_hi | addr_lo
        };

        target.set(addr);
        self.addr_latch.set(!latch);
    }

    fn vram_read_u8(&self, addr_space: &dyn AddressSpace) -> u8 {
        let addr = self.data_addr.get();
        let data = addr_space.read_u8(addr);
        let step = self.ctrl.get().vram_inc_step();
        self.data_addr.set(addr.wrapping_add(step));
        data
    }

    fn vram_write_u8(&self, addr_space: &dyn AddressSpace, value: u8) {
        let addr = self.data_addr.get();
        addr_space.write_u8(addr, value);
        let step = self.ctrl.get().vram_inc_step();
        self.data_addr.set(addr.wrapping_add(step));
    }

    pub fn decay_open_bus(&self) {
        let mut rng = thread_rng();
        for i in 0..8 {
            if rng.gen_bool(0.25) {
                self.open_bus.set(self.open_bus.get() & !(1 << i));
            }
        }
    }

    pub fn run<'a>(&'a self) -> impl Generator<Yield=PpuCycle, Return=()> + 'a {
        move || {
            loop {
                for scanline in 0..262u16 {
                    for dot in 0..341u16 {

                        if scanline == 241 && dot == 1 {
                            let mut status = self.status.get();
                            status.insert(PpuStatus::V);
                            self.status.set(status);
                        }
                        if scanline == 261 && dot == 1 {
                            let mut status = self.status.get();
                            status.remove(PpuStatus::V);
                            self.status.set(status);
                        }

                        yield PpuCycle::Tick;
                    }
                }
            }
        }
    }
}

pub enum PpuCycle {
    Tick
}

pub struct PpuAddressSpace {
    ram: Mirrored<Ram2KB>,
    palette: Mirrored<Ram32>,
    mapper: Rc<RefCell<Box<dyn Mapper>>>,
}

impl PpuAddressSpace {
    pub fn new(mapper: Rc<RefCell<Box<dyn Mapper>>>) -> Self {
        PpuAddressSpace {
            ram: Mirrored::new(Ram2KB::new(), 0x800, 0x2000),
            palette: Mirrored::new(Ram32::new(), 0x20, 0x3F00),
            mapper,
        }
    }
}

impl AddressSpace for PpuAddressSpace {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.mapper.borrow().ppu_addr_space().read_u8(addr),
            0x2000..=0x3EFF => self.ram.read_u8(addr),
            0x3F00..=0x3FFF => self.palette.read_u8(addr),
            _ => invalid_address!(addr)
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => self.mapper.borrow().ppu_addr_space().write_u8(addr, value),
            0x2000..=0x3EFF => self.ram.write_u8(addr, value),
            0x3F00..=0x3FFF => self.palette.write_u8(addr, value),
            _ => invalid_address!(addr)
        }
    }
}

pub struct MemoryMappedRegisters {
    ppu_addr_space: PpuAddressSpace,
    pub ppu: Ppu
}

impl MemoryMappedRegisters {
    pub fn new(ppu: Ppu, ppu_addr_space: PpuAddressSpace) -> Self {
        MemoryMappedRegisters { ppu_addr_space, ppu }
    }
}

impl AddressSpace for MemoryMappedRegisters {

    fn read_u8(&self, addr: u16) -> u8 {
        let result = match addr {
            0x2000 => self.ppu.open_bus.get(),
            0x2001 => self.ppu.open_bus.get(),
            0x2002 => self.ppu.status() | (self.ppu.open_bus.get() & 0b0001_1111),
            0x2003 => self.ppu.open_bus.get(),
            0x2005 => self.ppu.open_bus.get(),
            0x2006 => self.ppu.open_bus.get(),
            0x2004 => {
                // http://wiki.nesdev.com/w/index.php/PPU_OAM#Byte_2
                // bits 2-4 of byte 2 are "unimplemented" and thus, should be cleared
                let addr = self.ppu.oam_addr.get() as u16;
                let mask = if addr % 4 == 2 { 0b1110_0011 } else { 0xFF };
                self.ppu.oam_data.read_u8(addr) & mask
            },
            0x2007 => {
                let bus_mask = match self.ppu.data_addr.get() {
                    0x3F00..=0x3FFF => self.ppu.open_bus.get() & 0b1100_0000, // palette values are 6bits wide
                    _ => 0x00
                };
                self.ppu.vram_read_u8(&self.ppu_addr_space) | bus_mask
            },
            _ => invalid_address!(addr)
        };
        self.ppu.open_bus.set(result);
        result
    }

    fn write_u8(&self, addr: u16, value: u8) {
        self.ppu.open_bus.set(value);
        match addr {
            0x2000 => self.ppu.ctrl.set(PpuCtrl::from_bits_truncate(value)),
            0x2001 => self.ppu.mask.set(PpuMask::from_bits_truncate(value)),
            0x2002 => (),
            0x2003 => self.ppu.oam_addr.set(value),
            0x2004 => {
                self.ppu.oam_data.write_u8(self.ppu.oam_addr.get() as u16, value);
                let addr = self.ppu.oam_addr.get();
                self.ppu.oam_addr.set(addr.wrapping_add(1));
            },
            0x2005 => self.ppu.latched_write(&self.ppu.scroll_addr, value),
            0x2006 => self.ppu.latched_write(&self.ppu.data_addr, value),
            0x2007 => self.ppu.vram_write_u8(&self.ppu_addr_space, value),
            _ => invalid_address!(addr)
        }
    }
}
