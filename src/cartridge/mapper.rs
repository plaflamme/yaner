use super::Mapper;
use super::rom::Rom;
use std::cell::Cell;

// http://wiki.nesdev.com/w/index.php/NROM
pub struct NROM {
    prg_ram: Cell<[u8; 2048]>,
    prg_rom: Vec<u8>,
}

impl From<&Rom> for NROM {
    fn from(rom: &Rom) -> Self {
        NROM {
            prg_ram: Cell::new([0; 2048]),
            prg_rom: rom.prg_rom.clone()
        }
    }
}

impl Mapper for NROM {

    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            // PRG RAM,
            0x6000..=0x7FFF => self.prg_ram.get()[(addr - 0x6000) as usize],
            // 0x8000..=0xBFFF - First 16 KB of ROM.
            // 0xC000..=0xFFFF - Last 16 KB of ROM (NROM-256) or mirror of $8000-$BFFF (NROM-128).
            0x8000..=0xFFFF => self.prg_rom[(addr - 0x8000) as usize % self.prg_rom.len()],
            _ => panic!("addressed an unmapped location 0x{:X?}", addr)
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        let mapped_addr = (addr - 0x6000) as usize;
        match addr {
            0x6000..=0x7FFF => {
                let work: &Cell<[u8]> = &self.prg_ram;
                let cells = work.as_slice_of_cells();
                cells[(addr - 0x6000) as usize].set(value);
            },
            0x8000..=0xFFFF => panic!("tried writing to a read only location 0x{:X?}", addr),
            _ => panic!("addressed an unmapped location 0x{:X?}", addr)
        }
    }
}
