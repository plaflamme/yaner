#![allow(non_upper_case_globals)]

use crate::cartridge::Mapper;
use crate::memory::Ram256;
use crate::memory::{AddressSpace, Mirrored, Ram2KB, Ram32};
use rand::{thread_rng, Rng};
use std::cell::{Cell, RefCell};
use std::ops::Generator;
use std::rc::Rc;

pub mod reg;
pub mod renderer;
pub mod vram_address;
pub mod debug;
pub mod rgb;

use reg::{PpuCtrl, PpuMask, Registers};
use renderer::Renderer;

// NOTES:
//   Nametable - this is stored in VRAM by the CPU. Each byte is an index into the pattern table.
//   Pattern table - these are 16 bytes values that contains the pattern that should be displayed on screen.
//     The 2 8-byte values are comined together to form 64 2-bit values (or 8x8 2-bit values)
//     The 2-bit values are indices into the color palette
//   Palettes - the NES has a static system palette that contains 64 colors. and each frame has its own palette which is a subset of the system palette.
//     Frame palette - the frame palette is 8 groups of 4 colours. Groups are indexed 0-7 and individual colors 0-3.
//       Palettes 0-3 are for the backgrounds, palettes 4-7 are for the sprites.
//       The frame palette is selected from the attribute table which is part of the nametable (thus also setup by the CPU, presumably during vblank)
//   Attribute table - the attribute table divides the frame into blocks of 4x4 tiles. Each block is also divided into 4 regions of 2x2 tiles.
//     Each byte in the attribute table represents a single block of 4x4 tiles.
//     Each byte is divided into 4 2-bit values, one per block region (2x2 tiles).
//       * top-left:  ------xx
//       * top-right: ----xx--
//       * bot-left:  --xx----
//       * bot-right: xx------

pub struct Ppu {
    pub bus: PpuBus,
    registers: Registers,

    // internal OAM memory, enough for 64 sprites of 4 bytes each
    oam_data: Ram256,

    renderer: Renderer,
}

impl Ppu {
    pub fn new(mapper: Rc<RefCell<Box<dyn Mapper>>>) -> Self {
        Ppu {
            bus: PpuBus::new(mapper),
            registers: Registers::new(),

            oam_data: Ram256::new(),

            renderer: Renderer::new(),
        }
    }

    // read PPUSTATUS with side effects
    fn read_status(&self) -> u8 {
        let mut status = self.registers.read_status();
        // allow the renderer to suppress vbl and nmi
        self.renderer.ppustatus_read(&mut status);
        status.bits()
    }

    // read from VRAM with side effects
    fn vram_read_u8(&self) -> u8 {
        let addr: u16 = self.registers.v_addr.get().into();
        let data = self.bus.read_u8(addr);
        let step = self.registers.ctrl.get().vram_inc_step();
        self.registers.v_addr.update(|mut v| {
            v.increment(step);
            v
        });
        data
    }

    // write to VRAM with side effects
    fn vram_write_u8(&self, value: u8) {
        let addr: u16 = self.registers.v_addr.get().into();
        self.bus.write_u8(addr, value);
        let step = self.registers.ctrl.get().vram_inc_step();
        self.registers.v_addr.update(|mut v| {
            v.increment(step);
            v
        });
    }

    pub fn run<'a>(&'a self) -> impl Generator<Yield = PpuCycle, Return = ()> + 'a {
        self.renderer.run(&self.registers, &self.bus)
    }
}

#[derive(Debug)]
pub enum PpuCycle {
    Tick,
    Nmi, // same as tick, but nmi should be triggered in the cpu
    Frame,
}

pub struct PpuBus {
    pub vram: Mirrored<Ram2KB>,
    palette: Mirrored<Ram32>,
    mapper: Rc<RefCell<Box<dyn Mapper>>>,
}

impl PpuBus {
    pub fn new(mapper: Rc<RefCell<Box<dyn Mapper>>>) -> Self {
        PpuBus {
            vram: Mirrored::new(Ram2KB::new(), 0x800, 0x2000),
            palette: Mirrored::new(Ram32::new(), 0x20, 0x3F00),
            mapper,
        }
    }
}

impl AddressSpace for PpuBus {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.mapper.borrow().ppu_addr_space().read_u8(addr),
            0x2000..=0x3EFF => self.vram.read_u8(addr),
            0x3F00..=0x3FFF => self.palette.read_u8(addr),
            _ => invalid_address!(addr),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => self.mapper.borrow().ppu_addr_space().write_u8(addr, value),
            0x2000..=0x3EFF => self.vram.write_u8(addr, value),
            0x3F00..=0x3FFF => self.palette.write_u8(addr, value),
            _ => invalid_address!(addr),
        }
    }
}

pub struct MemoryMappedRegisters {
    ppu: Rc<Ppu>,
    // http://wiki.nesdev.com/w/index.php/PPU_registers#Ports
    open_bus: Cell<u8>,
}

impl MemoryMappedRegisters {
    pub fn new(ppu: Rc<Ppu>) -> Self {
        MemoryMappedRegisters { ppu, open_bus: Cell::default() }
    }

    pub fn decay_open_bus(&self) {
        let mut rng = thread_rng();
        for i in 0..8 {
            if rng.gen_bool(0.25) {
                self.open_bus.set(self.open_bus.get() & !(1 << i));
            }
        }
    }
}

impl AddressSpace for MemoryMappedRegisters {
    fn read_u8(&self, addr: u16) -> u8 {
        let result = match addr {
            0x2000 => self.open_bus.get(),
            0x2001 => self.open_bus.get(),
            0x2002 => self.ppu.read_status() | (self.open_bus.get() & 0b0001_1111),
            0x2003 => self.open_bus.get(),
            0x2004 => {
                // http://wiki.nesdev.com/w/index.php/PPU_OAM#Byte_2
                // bits 2-4 of byte 2 are "unimplemented" and thus, should be cleared
                let addr = self.ppu.registers.oam_addr.get() as u16;
                let mask = if addr % 4 == 2 { 0b1110_0011 } else { 0xFF };
                self.ppu.oam_data.read_u8(addr) & mask
            }
            0x2005 => self.open_bus.get(),
            0x2006 => self.open_bus.get(),
            0x2007 => {
                let bus_mask = match self.ppu.registers.v_addr.get().into() {
                    0x3F00..=0x3FFF => self.open_bus.get() & 0b1100_0000, // palette values are 6bits wide
                    _ => 0x00,
                };
                self.ppu.vram_read_u8() | bus_mask
            }
            _ => invalid_address!(addr),
        };
        self.open_bus.set(result);
        result
    }

    fn write_u8(&self, addr: u16, value: u8) {
        self.open_bus.set(value);
        match addr {
            0x2000 => {
                self.ppu.registers.ctrl.set(PpuCtrl::from_bits_truncate(value));
                // writing to the control register also sets the resulting nametable bits in t_addr
                self.ppu.registers.t_addr.update(|mut t| {
                    t.set_nametable(value & 0b0000_0011);
                    t
                });
            },
            0x2001 => self.ppu.registers.mask.set(PpuMask::from_bits_truncate(value)),
            0x2002 => (),
            0x2003 => self.ppu.registers.oam_addr.set(value),
            0x2004 => {
                self.ppu
                    .oam_data
                    .write_u8(self.ppu.registers.oam_addr.get() as u16, value);
                self.ppu.registers.oam_addr.update(|addr| addr.wrapping_add(1));
            }
            0x2005 => self.ppu.registers.write_scroll(value),
            0x2006 => self.ppu.registers.write_addr(value),
            0x2007 => self.ppu.vram_write_u8(value),
            _ => invalid_address!(addr),
        }
    }
}
