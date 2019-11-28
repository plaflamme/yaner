use std::cell::Cell;

pub trait AddressSpace {
    fn read_u8(&self, addr: u16) -> u8;
    fn write_u8(&self, addr: u16, value: u8);

    fn read_u16(&self, addr: u16) -> u16 {
        let low = self.read_u8(addr) as u16;
        let high = self.read_u8(addr.wrapping_add(1)) as u16;
        (high << 8) | low
    }

    fn write_u16(&self, addr: u16, v: u16) {
        let low = (v & 0x00FF) as u8;
        let high = (v >> 8) as u8;
        self.write_u8(addr, low);
        self.write_u8(addr.wrapping_add(1), high);
    }
}

// https://github.com/rust-lang/rust/issues/43408
pub struct Ram2KB {
    data: Cell<[u8; 0x0800]>
}

impl Ram2KB {
    pub fn new() -> Self {
        Ram2KB { data: Cell::new([0; 0x0800]) }
    }
}

impl AddressSpace for Ram2KB {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data.get()[addr as usize]
    }

    fn write_u8(&self, addr: u16, value: u8) {
        // TODO: for some reason as_slice_of_cells can only be invoked on &Cell<[u8]>
        let ram: &Cell<[u8]> = &self.data;
        let cells = ram.as_slice_of_cells();
        cells[addr as usize].set(value);
    }
}
