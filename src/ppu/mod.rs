#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::Cell;
use crate::memory::{AddressSpace, Ram2KB, Mirrored, NullAddressSpace};
use crate::memory::Ram256;
use std::ops::Generator;

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

    ppu_ram: Ram2KB, // VRAM
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

            ppu_ram: Ram2KB::new(),
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

    fn latched_write(&self, target: &Cell<u16>, value: u8) {
        let latch = self.addr_latch.get();
        let current = target.get();

        let addr = if latch {
            let addr_lo = current & 0x00FF;
            let addr_hi = (value as u16) << 8;
            addr_hi | addr_lo
        } else {
            let addr_lo = value as u16;
            let addr_hi = current & 0xFF00;
            addr_hi | addr_lo
        };

        target.set(addr);
        self.addr_latch.set(!latch);
    }

    pub fn run<'a>(&'a self, _addr_space: &'a dyn AddressSpace) -> impl Generator<Yield=PpuCycle, Return=()> + 'a {
        move || {
            loop {
                yield PpuCycle::Tick;
            }
        }
    }
}

pub enum PpuCycle {
    Tick
}

// TODO: this is the Cpu's address space. This should probably be exposed as public methods to CpuAddressSpace
impl AddressSpace for Ppu {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x2000 => self.ctrl.get().bits(),
            0x2001 => self.mask.get().bits(),
            0x2002 => self.status(),
            0x2003 => self.oam_addr.get(),
            0x2004 => self.oam_data.read_u8(self.oam_addr.get() as u16),
            _ => unimplemented!()
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x2000 => self.ctrl.set(PpuCtrl::from_bits_truncate(value)),
            0x2001 => self.mask.set(PpuMask::from_bits_truncate(value)),
            0x2003 => self.oam_addr.set(value),
            0x2004 => {
                self.oam_data.write_u8(self.oam_addr.get() as u16, value);
                let addr = self.oam_addr.get();
                self.oam_addr.set(addr.wrapping_add(1));
            },
            0x2005 => self.latched_write(&self.scroll_addr, value),
            0x2006 => self.latched_write(&self.data_addr, value),
            _ => unimplemented!()
        }
    }
}

pub struct PpuAddressSpace<'a> {
    ppu_ram: Box<dyn AddressSpace + 'a>,
    mapper: &'a dyn AddressSpace,
    palette_ctrl: &'a dyn AddressSpace
}

impl<'a> PpuAddressSpace<'a> {
    pub fn new(ppu: &'a Ppu, mapper: &'a dyn AddressSpace) -> Self {
        PpuAddressSpace {
            ppu_ram: Box::new(Mirrored::new(&ppu.ppu_ram, 0x800, 0x2000)),
            mapper,
            palette_ctrl: &NullAddressSpace
        }
    }
}

impl<'a> AddressSpace for PpuAddressSpace<'a> {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.mapper.read_u8(addr),
            0x2000..=0x3EFF => self.ppu_ram.read_u8(addr),
            0x3F00..=0x3FFF => self.palette_ctrl.read_u8(addr),
            _ => unimplemented!()
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => self.mapper.write_u8(addr, value),
            0x2000..=0x3EFF => self.ppu_ram.write_u8(addr, value),
            0x3F00..=0x3FFF => self.palette_ctrl.write_u8(addr, value),
            _ => unimplemented!()
        }
    }
}
