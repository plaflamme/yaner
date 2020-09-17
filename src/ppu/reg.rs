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
        const H = 1 << 5; // Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
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

    pub fn sprite_pattern_table_address(&self) -> u16 {
        // (0: $0000; 1: $1000)
        if self.contains(PpuCtrl::S) {
            0x1000
        } else {
            0x0000
        }
    }

    pub fn sprite_height(&self) -> u8 {
        if self.contains(PpuCtrl::H) {
            16
        } else {
            8
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

    // handles writes to PPUSCROLL
    // http://wiki.nesdev.com/w/index.php/PPU_registers#PPUSCROLL
    // See writes to $2005 here http://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
    pub fn write_scroll(&self, value: u8) {
        if self.addr_latch.get() {
            // t: CBA..HG FED..... = d: HGFEDCBA
            self.t_addr.update(|mut t| {
                t.set_fine_y(value & 0b0000_0111);
                t.set_coarse_y(value >> 3);
                t
            });
        } else {
            // t: ....... ...HGFED = d: HGFED...
            // x:              CBA = d: .....CBA
            self.fine_x.set(value & 0b0000_0111);
            self.t_addr.update(|mut t| {
                t.set_coarse_x(value >> 3);
                t
            });
        }
        self.addr_latch.update(|latch| !latch);
    }

    // handles writes to PPUADDR
    // http://wiki.nesdev.com/w/index.php/PPU_registers#PPUADDR
    // See writes to $2006 here http://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
    pub fn write_addr(&self, value: u8) {
        let u16_value = value as u16;
        if self.addr_latch.get() {
            // t: ....... HGFEDCBA = d: HGFEDCBA
            // v                   = t
            self.t_addr.update(|v| (v & 0xFF00u16) | u16_value);
            self.v_addr.set(self.t_addr.get());
        } else {
            // t: .FEDCBA ........ = d: ..FEDCBA
            // t: X...... ........ = 0
            self.t_addr.update(|v| (v & 0x80FFu16) | ((u16_value & 0x003F) << 8));
        }
        self.addr_latch.update(|latch| !latch);
    }

    // handles side-effects of reading or writing to PPUDATA ($2007)
    // http://wiki.nesdev.com/w/index.php/PPU_registers#PPUADDR
    // NOTE: this does not actually read or write from the VRAM, it returns the address to read or write.
    pub fn rw_vram_addr(&self) -> u16 {
        let addr: u16 = self.v_addr.get().into();
        let step = self.ctrl.get().vram_inc_step();
        self.v_addr.update(|mut v| {
            v.increment(step);
            v
        });
        addr
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

    #[test]
    fn test_registers_read_status_clears_vblank() {
        let registers = Registers::new();
        registers.status.set(PpuStatus::V | PpuStatus::O);
        let read = registers.read_status();
        assert!(read.contains(PpuStatus::V | PpuStatus::O));
        assert_eq!(registers.status.get(), PpuStatus::O);
    }

    #[test]
    fn test_registers_read_status_resets_latch() {
        let registers = Registers::new();
        registers.addr_latch.set(true);
        registers.read_status();
        assert!(!registers.addr_latch.get());
    }

    #[test]
    fn test_registers_first_write_scroll() {
        let registers = Registers::new();
        registers.addr_latch.set(false);
        registers.write_scroll(0b10101_101);
        let t_addr: u16 = registers.t_addr.get().into();
        assert_eq!(t_addr, 0b000_00_00000_10101);
        assert_eq!(registers.fine_x.get(), 0b0000_0101);
        assert!(registers.addr_latch.get());
    }

    #[test]
    fn test_registers_second_write_scroll() {
        let registers = Registers::new();
        registers.addr_latch.set(true);
        registers.write_scroll(0b10101_101);
        let t_addr: u16 = registers.t_addr.get().into();
        assert_eq!(t_addr, 0b101_00_10101_00000);
        assert!(!registers.addr_latch.get());
    }

    #[test]
    fn test_registers_first_write_addr() {
        let registers = Registers::new();
        registers.addr_latch.set(false);
        registers.write_addr(0b11_101010);
        let t_addr: u16 = registers.t_addr.get().into();
        assert_eq!(t_addr, 0b0_0101010_0000_0000);
        assert!(registers.addr_latch.get());

        registers.addr_latch.set(false);
        registers.t_addr.set(VramAddress::from(0b0_1000000_0000_0000));
        registers.write_addr(0b11_010101);
        let t_addr: u16 = registers.t_addr.get().into();
        assert_eq!(t_addr, 0b0_0010101_0000_0000);
        assert!(registers.addr_latch.get());
    }

    #[test]
    fn test_registers_second_write_addr() {
        let registers = Registers::new();
        registers.t_addr.set(VramAddress::from(0b0_0010101_0000_0000));
        registers.v_addr.set(VramAddress::from(0b1_0101010_1010_1010));
        registers.addr_latch.set(true);
        registers.write_addr(0b1111_1010);

        let t_addr: u16 = registers.t_addr.get().into();
        assert_eq!(t_addr, 0b0_0010101_1111_1010, "incorrect value for t_addr");

        let v_addr: u16 = registers.v_addr.get().into();
        assert_eq!(v_addr, 0b0_0010101_1111_1010, "incorrect value for v_addr");

        assert!(!registers.addr_latch.get());
    }

    #[test]
    fn test_registers_rw_vram_addr() {
        let registers = Registers::new();
        registers.v_addr.set(VramAddress::from(0b1_0101010_1010_1010));

        let addr = registers.rw_vram_addr();
        assert_eq!(addr, 0b1_0101010_1010_1010);

        let addr: u16 = registers.v_addr.get().into();
        assert_eq!(addr, 0b1_0101010_1010_1011);

        registers.ctrl.update(|ctrl| ctrl | PpuCtrl::I);

        registers.v_addr.set(VramAddress::from(0b1_0101010_1010_1010));

        let addr = registers.rw_vram_addr();
        assert_eq!(addr, 0b1_0101010_1010_1010);

        let addr: u16 = registers.v_addr.get().into();
        assert_eq!(addr, 0b1_0101010_1010_1010 + 32);
    }
}
