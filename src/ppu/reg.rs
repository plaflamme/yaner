use std::cell::Cell;
use bitflags::bitflags;

use super::vram_address::VramAddress;

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
    pub fn vram_inc_step(&self) -> u16 {
        // (0: add 1, going across; 1: add 32, going down)
        if self.contains(PpuCtrl::I) {
            32
        } else {
            1
        }
    }

    pub fn bg_pattern_table_address(&self) -> u16 {
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

impl PpuMask {
    pub fn is_rendering(&self) -> bool {
        self.contains(PpuMask::b) | self.contains(PpuMask::s)
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

pub struct Registers {
    pub ctrl: Cell<PpuCtrl>,
    pub mask: Cell<PpuMask>,
    pub status: Cell<PpuStatus>,

    pub oam_addr: Cell<u8>, // OAMADDR

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#PPU_internal_registers
    // Accessed through PPUSCROLL and PPUADDR
    pub t_addr: Cell<VramAddress>,
    pub v_addr: Cell<VramAddress>,
    addr_latch: Cell<bool>, // this is referred to as w in the wiki

    pub fine_x: Cell<u8>,
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            ctrl: Cell::new(PpuCtrl::default()),
            mask: Cell::new(PpuMask::default()),
            status: Cell::new(PpuStatus::default()),
            oam_addr: Cell::new(0x00),

            t_addr: Cell::new(VramAddress::default()),
            v_addr: Cell::new(VramAddress::default()),
            addr_latch: Cell::new(false),
            fine_x: Cell::new(0),
        }
    }

    // read PPUSTATUS, with side effects
    pub fn read_status(&self) -> PpuStatus {
        let status = self.status.get();
        // reading PPUSTATUS always clears VBL
        self.status.update(|s| s - PpuStatus::V);
        // reading PPUSTATUS resets the address latch
        self.addr_latch.set(false);
        status
    }

    pub fn write_scroll(&self, value: u8) {
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

    pub fn write_addr(&self, value: u8) {
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
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ctrl_vram_address_increment() {
        let mut ctrl = PpuCtrl::default();
        assert_eq!(ctrl.vram_inc_step(), 1);
        ctrl.toggle(PpuCtrl::I);
        assert_eq!(ctrl.vram_inc_step(), 32);
    }

    #[test]
    fn test_ctrl_background_pattern_table() {
        let mut ctrl = PpuCtrl::default();
        assert_eq!(ctrl.bg_pattern_table_address(), 0x0000);
        ctrl.toggle(PpuCtrl::B);
        assert_eq!(ctrl.bg_pattern_table_address(), 0x1000);
    }

    #[test]
    fn test_mask_is_rendering() {
        let mut mask = PpuMask::default();
        assert_eq!(mask.is_rendering(), false);
        mask.toggle(PpuMask::s);
        assert_eq!(mask.is_rendering(), true);
        mask.toggle(PpuMask::b);
        assert_eq!(mask.is_rendering(), true);
        mask.toggle(PpuMask::s);
        assert_eq!(mask.is_rendering(), true);
        mask.toggle(PpuMask::b);
        assert_eq!(mask.is_rendering(), false);
    }

    #[test]
    fn test_status_default() {
        let status = PpuStatus::default();
        assert_eq!(status.contains(PpuStatus::V | PpuStatus::O), true);
        assert_eq!(status.contains(PpuStatus::S), false);
    }

}
