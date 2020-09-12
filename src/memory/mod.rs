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

pub struct Rom {
    data: Vec<u8>,
}

impl Rom {
    pub fn new(data: Vec<u8>) -> Self {
        Rom { data }
    }
}

impl AddressSpace for Rom {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data[addr as usize % self.data.len()]
    }

    fn write_u8(&self, _: u16, _: u8) {}
}

fn write_u8(data: &Cell<[u8]>, addr: u16, value: u8) {
    // TODO: for some reason as_slice_of_cells can only be invoked on &Cell<[u8]>
    let cells = data.as_slice_of_cells();
    cells[addr as usize].set(value);
}

pub struct Ram {
    data: Vec<Cell<u8>>,
}

impl Ram {
    pub fn new(size: usize) -> Self {
        Ram {
            data: vec![Cell::new(0u8); size],
        }
    }
}

impl AddressSpace for Ram {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data[addr as usize % self.data.len()].get()
    }

    fn write_u8(&self, addr: u16, value: u8) {
        self.data[addr as usize % self.data.len()].set(value)
    }
}

// https://github.com/rust-lang/rust/issues/43408
pub struct Ram2KB {
    data: Cell<[u8; 0x0800]>,
}

impl Ram2KB {
    pub fn new() -> Self {
        Ram2KB {
            data: Cell::new([0; 0x0800]),
        }
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

// https://github.com/rust-lang/rust/issues/43408
pub struct Ram8KB {
    data: Cell<[u8; 0x2000]>,
}

impl Ram8KB {
    pub fn new() -> Self {
        Ram8KB {
            data: Cell::new([0; 0x2000]),
        }
    }
}

impl AddressSpace for Ram8KB {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data.get()[addr as usize]
    }

    fn write_u8(&self, addr: u16, value: u8) {
        write_u8(&self.data, addr, value);
    }
}

pub struct Ram256 {
    data: Cell<[u8; 0x0100]>,
}

impl Ram256 {
    pub fn new() -> Self {
        Ram256 {
            data: Cell::new([0; 0x0100]),
        }
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

pub struct Ram32 {
    data: Cell<[u8; 0x20]>,
}

impl Ram32 {
    pub fn new() -> Self {
        Ram32 {
            data: Cell::new([0; 0x20]),
        }
    }
}

impl AddressSpace for Ram32 {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data.get()[addr as usize]
    }

    fn write_u8(&self, addr: u16, value: u8) {
        write_u8(&self.data, addr, value);
    }
}

pub struct Mirrored<T: AddressSpace> {
    addr_space: T,
    size: u16,
    base_addr: u16,
}

impl<T: AddressSpace> Mirrored<T> {
    pub fn new(addr_space: T, size: u16, base_addr: u16) -> Self {
        Mirrored {
            addr_space,
            size,
            base_addr,
        }
    }

    fn translate(&self, addr: u16) -> u16 {
        (addr.saturating_sub(self.base_addr)) % self.size
    }
}

impl<T: AddressSpace> AddressSpace for Mirrored<T> {
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
        0x00
    }

    fn write_u8(&self, _: u16, _: u8) {
        ()
    }
}
