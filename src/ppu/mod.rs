use bitflags::bitflags;
use std::cell::Cell;
use crate::memory::AddressSpace;

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

bitflags! {
    // http://wiki.nesdev.com/w/index.php/PPU_programmer_reference#PPUSTATUS
    struct PpuStatus: u8 {
        const O = 1 << 5; // Sprite overflow
        const S = 1 << 6; // Sprite 0 Hit
        const V = 1 << 7; // vblank
    }
}

pub struct Ppu {
    ctrl: Cell<PpuCtrl>,
    status: Cell<PpuStatus>
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            ctrl: Cell::new(PpuCtrl::empty()),
            status: Cell::new(PpuStatus::empty()),
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
            0x2002 => self.status(),
            _ => unimplemented!()
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x2000 => self.ctrl.set(PpuCtrl::from_bits_truncate(value)),
            _ => unimplemented!()
        }
    }
}
