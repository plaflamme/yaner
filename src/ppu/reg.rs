use bitflags::bitflags;

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ctrl_vram_address_increment() {
        let mut ctrl = PpuCtrl::default();
        assert_eq!(ctrl.vram_inc_step(), 1);
        ctrl.set(PpuCtrl::I, true);
        assert_eq!(ctrl.vram_inc_step(), 32);
    }

    #[test]
    fn test_ctrl_background_pattern_table() {
        let mut ctrl = PpuCtrl::default();
        assert_eq!(ctrl.bg_pattern_table_address(), 0x0000);
        ctrl.set(PpuCtrl::B, true);
        assert_eq!(ctrl.bg_pattern_table_address(), 0x1000);
    }

}
