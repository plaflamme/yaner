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

fn write_u8(data: &Cell<[u8]>, addr: u16, value: u8) {
    // TODO: for some reason as_slice_of_cells can only be invoked on &Cell<[u8]>
    let cells = data.as_slice_of_cells();
    cells[addr as usize].set(value);
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
        write_u8(&self.data, addr, value);
    }
}

pub struct Ram256 {
    data: Cell<[u8; 0x0100]>
}

impl Ram256 {
    pub fn new() -> Self {
        Ram256 { data: Cell::new([0; 0x0100]) }
    }
}

impl AddressSpace for Ram256 {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data.get()[addr as usize]
    }

    fn write_u8(&self, addr: u16, value: u8) {
        write_u8(&self.data, addr, value);
    }
}

pub struct Mirrored<'a> {
    addr_space: &'a dyn AddressSpace,
    size: u16,
    base_addr: u16,
}

impl<'a> Mirrored<'a> {
    pub fn new(addr_space: &'a dyn AddressSpace, size: u16, base_addr: u16) -> Self {
        Mirrored { addr_space, size, base_addr }
    }

    fn translate(&self, addr: u16) -> u16 {
        (addr - self.base_addr) % self.size
    }
}

impl<'a> AddressSpace for Mirrored<'a> {

    fn read_u8(&self, addr: u16) -> u8 {
        self.addr_space.read_u8(self.translate(addr))
    }

    fn write_u8(&self, addr: u16, value: u8) {
        self.addr_space.write_u8(self.translate(addr), value);
    }
}

pub struct NullAddressSpace;
impl AddressSpace for NullAddressSpace {
    fn read_u8(&self, _: u16) -> u8 {
        panic!("read from uninitialized memory")
    }

    fn write_u8(&self, _: u16, _: u8) {
        panic!("write to uninitialized memory")
    }
}
