pub trait AddressSpace {
    fn peek(&self, addr: u16) -> u8;
    fn store(&mut self, addr: u16, value: u8);

    fn peek16(&self, addr: u16) -> u16 {
        let low = self.peek(addr) as u16;
        let high = self.peek(addr + 1) as u16;
        (high << 8) | low
    }

    fn store16(&mut self, addr: u16, v: u16) {
        let low = (v & 0x00FF) as u8;
        let high = (v >> 8) as u8;
        self.store(addr, low);
        self.store(addr.wrapping_add(1), high);
    }
}

pub struct Ram {
    bytes: Vec<u8>
}

impl AddressSpace for Ram {
    fn peek(&self, addr: u16) -> u8 {
        self.bytes[addr as usize]
    }

    fn store(&mut self, addr: u16, value: u8) {
        self.bytes[addr as usize] = value;
    }
}

pub struct Rom {
    bytes: Vec<u8>
}

impl AddressSpace for Rom {
    fn peek(&self, addr: u16) -> u8 {
        self.bytes[addr as usize]
    }

    fn store(&mut self, addr: u16, value: u8) {
        panic!("ROM is unwritable.")
    }
}

// An address space that mirrors another http://wiki.nesdev.com/w/index.php/Mirroring
//   Note that the base region may be smaller than the mirrored region
pub struct MirroredAddressSpace {
    base: Box<dyn AddressSpace>, // the space being mirrored

    base_start: u16, // the start address of the range being mirrored
    base_width: u16, // the width of the range being mirrored

    mirrored_start: u16, // the mirrored start address
    mirrored_end: u16, // the mirrored end address
}

impl MirroredAddressSpace {

    fn new(base: Box<dyn AddressSpace>, base_start: u16, base_end: u16, mirrored_start: u16, mirrored_end: u16) -> Self {
        MirroredAddressSpace {
            base,
            base_start,
            base_width: (base_end - base_start) + 1,
            mirrored_start,
            mirrored_end
        }
    }

    fn map_addr(&self, addr: u16) -> u16 {
        if addr < self.mirrored_start || addr > self.mirrored_end {
            panic!("invalid mirrored address: 0x{:X?} is not within [0x{:X?},0x{:X?}]", addr, self.mirrored_start, self.mirrored_end);
        }

        let offset = addr - self.mirrored_start;
        if offset < self.base_width { self.base_start + offset } else {
            (offset % self.base_width) + self.base_start
        }
    }
}

impl AddressSpace for MirroredAddressSpace {

    fn peek(&self, addr: u16) -> u8 {
        self.base.peek(self.map_addr(addr))
    }

    fn store(&mut self, addr: u16, value: u8) {
        self.base.store(self.map_addr(addr), value)
    }
}

struct MappedRegion {
    start: u16,
    end: u16,
    mem: Box<dyn AddressSpace>,
    map_addr: bool // whether or not to offset addresses by `start`
}

impl MappedRegion {
    fn new(start: u16, end: u16, mem: Box<dyn AddressSpace>, map_address: bool) -> Self {
        MappedRegion { start, end, mem, map_addr: map_address }
    }

    fn map_addr(&self, addr: u16) -> u16 {
        if self.map_addr { addr - self.start } else { addr }
    }
}

pub struct MemoryMap {
    regions: Vec<MappedRegion>
}

impl MemoryMap {
    fn new() -> Self {
        MemoryMap { regions: Vec::new() }
    }

    fn locate(&self, addr: u16) -> Option<&MappedRegion> {
        self.regions.iter().find(|r| r.start <= addr && r.end >= addr)
    }

    // TODO: isn't there a better way? I guess we could use RefCells?
    fn locate_mut(&mut self, addr: u16) -> Option<&mut MappedRegion> {
        self.regions.iter_mut().find(|r| r.start <= addr && r.end >= addr)
    }

    fn map_space(&mut self, start: u16, end: u16, mem: Box<dyn AddressSpace>, map_address: bool) {
        self.regions.push(MappedRegion::new(start, end, mem, map_address));
    }

    // adds a region [start,end] that mirrors the provided space from base_start to base_end
    fn map_mirrored_space(&mut self, start: u16, end: u16, base_start: u16, base_end: u16, mem: Box<dyn AddressSpace>, map_address: bool) {
        let mirrored_space = MirroredAddressSpace::new(mem, base_start, base_end, start, end);
        self.regions.push(MappedRegion::new(start, end, Box::new(mirrored_space), map_address))
    }
}

impl AddressSpace for MemoryMap {
    fn peek(&self, addr: u16) -> u8 {
        match self.locate(addr) {
            None => unimplemented!(),
            Some(region) => region.mem.peek(region.map_addr(addr))
        }
    }

    fn store(&mut self, addr: u16, value: u8) {
        match self.locate_mut(addr) {
            None => unimplemented!(),
            Some(region) => region.mem.store(region.map_addr(addr), value)
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mirrored_space() {
        let ram = Ram { bytes: [0,1,2,3].to_vec() };
        assert_eq!(1, ram.peek(1));
        let mirrored = MirroredAddressSpace::new(Box::new(ram), 0, 3, 10, 17);

        for i in 10..=17 {
            assert_eq!(((i -10) % 4) as u8, mirrored.peek(i));
        }
    }
}
