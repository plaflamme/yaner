#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::Cell;
use crate::memory::AddressSpace;
use crate::memory::Ram256;

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

    oam_data: Ram256
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            ctrl: Cell::new(PpuCtrl::default()),
            mask: Cell::new(PpuMask::default()),
            status: Cell::new(PpuStatus::default()),
            oam_addr: Cell::new(0x00),

            oam_data: Ram256::new(),
        }
    }

    fn status(&self) -> u8 {
        let status = self.status.get();
        let mut cleared = PpuStatus::from(status);
        cleared.remove(PpuStatus::V);
        self.status.set(cleared);
        status.bits()
    }
}

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
                self.oam_data.write_u8(self.oam_addr.get() as u16, value)
                let addr = self.oam_addr.get();
                self.oam_addr.set(addr.wrapping_add(1));
            },
            _ => unimplemented!()
        }
    }
}
