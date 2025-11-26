#![allow(clippy::unusual_byte_groupings)]

use bitregions::bitregions;

bitregions! {
    pub VramAddress u16 {
        FINE_Y:    0b111_00_00000_00000, // Fine y scroll
        NAMETABLE: 0b000_11_00000_00000, // Nametable select
        COARSE_Y:  0b000_00_11111_00000, // Coarse y scroll
        COARSE_X:  0b000_00_00000_11111, // Coarse x scroll
    }
}

impl VramAddress {
    pub fn increment(&mut self, step: u16) {
        self.0 = self.0.saturating_add(step);
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Tile_and_attribute_fetching
    // this value as an address into the nametable
    pub fn nametable_addr(&self, base_addr: u16) -> u16 {
        base_addr | (self.0 & 0x0FFF)
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Tile_and_attribute_fetching
    // this value as an address into the attribute table
    pub fn attribute_addr(&self) -> u16 {
        let v = self.0;
        0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)
    }

    pub fn copy_horizontal_bits(&mut self, from: &VramAddress) {
        self.0 &= !0b000_01_00000_11111;
        self.0 |= from.0 & 0b000_01_00000_11111;
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#During_dots_280_to_304_of_the_pre-render_scanline_.28end_of_vblank.29
    pub fn copy_vertical_bits(&mut self, from: &VramAddress) {
        self.0 &= !0b111_10_11111_00000;
        self.0 |= from.0 & 0b111_10_11111_00000;
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Coarse_X_increment
    pub fn incr_x(&mut self) {
        if self.coarse_x() == 31 {
            self.set_coarse_x(0u16);
            self.0 ^= 0x0400;
        } else {
            self.set_coarse_x(self.coarse_x() + 1);
        }
    }

    // https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Y_increment
    pub fn incr_y(&mut self) {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_vram_address_increment() {
        let mut addr = VramAddress::default();
        assert_eq!(addr.0, 0);
        addr.increment(16);
        assert_eq!(addr.0, 16);
        addr.increment(8);
        assert_eq!(addr.0, 24);
        addr.increment(0xFFFF);
        assert_eq!(addr.0, 0xFFFF);
    }

    #[test]
    fn test_vram_address_nametable_addr() {
        let addr = VramAddress::default();
        assert_eq!(addr.nametable_addr(0x2000), 0x2000);
        let addr = VramAddress::new(0xFFAB_u16);
        assert_eq!(addr.nametable_addr(0x2000), 0x2FAB);
        let addr = VramAddress::new(0x0030_u16);
        assert_eq!(addr.nametable_addr(0x2C00), 0x2C30);
    }

    #[test]
    fn test_vram_address_attribute_addr() {
        let addr = VramAddress::default();
        assert_eq!(addr.attribute_addr(), 0x23C0);

        // TODO: use proptest?
        let addr = VramAddress::new(0b1010_10_10101_01010u16);
        // compose the attribute address like so:
        //  NN 1111 YYY XXX
        //  || |||| ||| +++-- high 3 bits of coarse X (x/4)
        //  || |||| +++------ high 3 bits of coarse Y (y/4)
        //  || ++++---------- attribute offset (960 bytes)
        //  ++--------------- nametable select
        let high_x = addr.coarse_x() as u16 >> 2;
        let high_y = (addr.coarse_y() as u16 >> 2) << 3;
        let attr_offs = 0b1111_000_000;
        let nt = (addr.nametable() as u16) << 10;

        assert_eq!(
            addr.attribute_addr(),
            0x23C0 | nt | attr_offs | high_y | high_x
        );
    }

    #[test]
    fn test_vram_address_copy_vertical_bits() {
        // Vertical bits mask is 0b111_10_11111_00000

        let mut addr = VramAddress::default();
        let other = VramAddress::default();
        addr.copy_vertical_bits(&other);
        assert_eq!(addr.0, 0);
        addr.copy_vertical_bits(&VramAddress::new(0b111_10_11111_00000u16));
        assert_eq!(addr.0, 0b111_10_11111_00000u16);

        let mut addr = VramAddress::default();
        let other = VramAddress::new(0b101_01_01010_10101u16);
        addr.copy_vertical_bits(&other);
        assert_eq!(addr.0, 0b101_00_01010_00000);

        let mut addr = VramAddress::new(0b000_01_00000_10101u16);
        let other = VramAddress::new(0b101_00_01010_00000u16);
        addr.copy_vertical_bits(&other);
        assert_eq!(addr.0, 0b101_01_01010_10101);

        let mut addr = VramAddress::new(0b010_10_10101_00000u16);
        let other = VramAddress::new(0b101_00_01010_00000u16);
        addr.copy_vertical_bits(&other);
        assert_eq!(addr.0, 0b101_00_01010_00000);
    }

    #[test]
    fn test_vram_address_copy_horizontal_bits() {
        // Horizontal bits mask is 0b000_01_00000_11111

        let mut addr = VramAddress::default();
        let other = VramAddress::default();
        addr.copy_horizontal_bits(&other);
        assert_eq!(addr.0, 0);
        addr.copy_horizontal_bits(&VramAddress::new(0b000_01_00000_11111u16));
        assert_eq!(addr.0, 0b000_01_00000_11111u16);

        let mut addr = VramAddress::default();
        let other = VramAddress::new(0b101_01_01010_10101u16);
        addr.copy_horizontal_bits(&other);
        assert_eq!(addr.0, 0b000_01_00000_10101u16);

        let mut addr = VramAddress::new(0b101_01_01010_10101u16);
        let other = VramAddress::new(0b101_00_01010_01010u16);
        addr.copy_horizontal_bits(&other);
        assert_eq!(addr.0, 0b101_00_01010_01010u16);
    }

    #[test]
    fn test_vram_address_incr_x() {
        let mut addr = VramAddress::default();
        assert_eq!(addr.coarse_x(), 0);
        assert_eq!(addr.nametable(), 0);
        addr.incr_x();
        assert_eq!(addr.coarse_x(), 1);
        assert_eq!(addr.nametable(), 0);

        addr.set_coarse_x(31u8);
        addr.incr_x();
        assert_eq!(addr.coarse_x(), 0);
        assert_eq!(addr.nametable(), 1);

        addr.set_coarse_x(31u8);
        addr.incr_x();
        assert_eq!(addr.coarse_x(), 0);
        assert_eq!(addr.nametable(), 0);
    }

    #[test]
    fn test_vram_address_incr_y() {
        let mut addr = VramAddress::default();
        assert_eq!(addr.fine_y(), 0);
        assert_eq!(addr.coarse_y(), 0);
        assert_eq!(addr.nametable(), 0);

        addr.incr_y();
        assert_eq!(addr.fine_y(), 1);
        assert_eq!(addr.coarse_y(), 0);
        assert_eq!(addr.nametable(), 0);

        addr.set_fine_y(7u8);
        addr.incr_y();
        assert_eq!(addr.fine_y(), 0);
        assert_eq!(addr.coarse_y(), 1);
        assert_eq!(addr.nametable(), 0);

        addr.set_fine_y(7u8);
        addr.set_coarse_y(29u8);
        addr.incr_y();
        assert_eq!(addr.fine_y(), 0);
        assert_eq!(addr.coarse_y(), 0);
        assert_eq!(addr.nametable(), 2);

        addr.set_fine_y(7u8);
        addr.set_coarse_y(29u8);
        addr.incr_y();
        assert_eq!(addr.fine_y(), 0);
        assert_eq!(addr.coarse_y(), 0);
        assert_eq!(addr.nametable(), 0);

        // nametable doesn't switch in this case
        addr.set_fine_y(7u8);
        addr.set_coarse_y(31u8);
        addr.incr_y();
        assert_eq!(addr.fine_y(), 0);
        assert_eq!(addr.coarse_y(), 0);
        assert_eq!(addr.nametable(), 0);
    }
}
