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

#[derive(Clone)]
pub struct Dyn {
    data: Vec<Cell<u8>>,
    ro: bool,
}

impl Dyn {
    pub fn new(data: Vec<u8>) -> Self {
        Self {
            data: data.into_iter().map(Cell::new).collect(),
            ro: true,
        }
    }

    pub fn with_capacity(size: usize) -> Self {
        Self {
            data: vec![Cell::new(0u8); size],
            ro: false,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl AddressSpace for Dyn {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data[addr as usize % self.data.len()].get()
    }

    fn write_u8(&self, addr: u16, value: u8) {
        if !self.ro {
            self.data[addr as usize % self.data.len()].set(value)
        }
    }
}

pub struct Ram<const N: usize> {
    data: Cell<[u8; N]>,
}

impl<const N: usize> Ram<N> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<const N: usize> Default for Ram<N> {
    fn default() -> Self {
        Self {
            data: Cell::new([0; N]),
        }
    }
}

impl<const N: usize> AddressSpace for Ram<N> {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data.get()[addr as usize]
    }

    fn write_u8(&self, addr: u16, value: u8) {
        write_u8(&self.data, addr, value);
    }
}

pub type Ram32 = Ram<0x0020>;
pub type Ram256 = Ram<0x0100>;
pub type Ram2KB = Ram<0x0800>;
pub type Ram8KB = Ram<0x2000>;

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

    fn write_u8(&self, _: u16, _: u8) {}
}
