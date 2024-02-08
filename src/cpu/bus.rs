use crate::apu::Apu;
use crate::memory::{AddressSpace, Ram2KB};
use std::cell::{Cell, RefCell};

use crate::cartridge::Mapper;
use crate::input::Input;
use crate::ppu::PpuRegisters;
use std::rc::Rc;

use super::Interrupt;

// https://www.nesdev.org/wiki/NMI
#[derive(Default)]
struct NmiState {
    line: Cell<bool>,
    state: Cell<bool>,
}

impl NmiState {
    // Pull the nmi line.
    // The NMI unit is edge-sensitive, meaning that it reacts to high-to-low transitions in the signal.
    //   When the line changes from false to true, the internal state is set to true,
    //   but setting it from true to true will not alter the internal state (i.e: it will remain false if it was false).
    pub fn set_nmi_line(&self) {
        let prev_mni = self.line.get();
        if !prev_mni {
            self.state.set(true);
        }
        self.line.set(true);
    }

    pub fn clear_nmi_line(&self) {
        self.line.set(false);
        self.state.set(false);
    }

    // polling the nmi state clears it
    pub fn poll_nmi_state(&self) -> bool {
        let state = self.state.get();
        self.state.set(false);
        state
    }
}

// http://wiki.nesdev.com/w/index.php/2A03
pub struct IoRegisters {
    apu: Rc<Apu>,
    input1: Rc<dyn Input>,
    input2: Rc<dyn Input>,

    // OUT0-OUT2 latch
    out_latch: Cell<u8>,

    dma_latch: Cell<Option<u8>>,
}

impl IoRegisters {
    pub fn new(apu: Rc<Apu>, input1: Rc<dyn Input>, input2: Rc<dyn Input>) -> Self {
        IoRegisters {
            apu,
            input1,
            input2,
            out_latch: Cell::default(),
            dma_latch: Cell::new(None),
        }
    }

    // This will return last write to OAM DMA and then None until the next write
    pub fn dma_latch(&self) -> Option<u8> {
        self.dma_latch.take()
    }
}

impl AddressSpace for IoRegisters {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            // In the NES and Famicom, the top three (or five) bits are not driven, and so retain the bits of the previous byte on the bus.
            // Usually this is the most significant byte of the address of the controller port—0x40.
            // Certain games (such as Paperboy) rely on this behavior and require that reads from the controller ports return exactly $40 or $41 as appropriate.
            0x4016 => self.input1.read() | 0x40, // joy1
            0x4017 => self.input2.read() | 0x40, // joy2

            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.
            _ => 0x0,
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x4014 => self.dma_latch.set(Some(value)),
            0x4016 => {
                self.out_latch.set(value & 0x7); // lower 3 bits

                // The first bit is connected to the inputs
                // TODO: is this supposed to happen now or on the next tick?
                let out0 = value & 0x01;
                self.input1.strobe(out0);
                self.input2.strobe(out0);
            }
            0x4000..=0x4013 | 0x4015 | 0x4017 => self.apu.write_u8(addr, value),
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.
            _ => (),
        }
    }
}

pub struct CpuBus {
    pub ram: Ram2KB,
    pub io_regsiters: IoRegisters,
    pub ppu_registers: PpuRegisters,
    pub mapper: Rc<RefCell<Box<dyn Mapper>>>,
    pub intr: Cell<Option<Interrupt>>,

    // NMI is edge-sensitive
    nmi_state: NmiState,
    // IRQ is level-sensitive
    irq_line: Cell<bool>,
}

impl CpuBus {
    pub fn new(
        io_regsiters: IoRegisters,
        ppu_registers: PpuRegisters,
        mapper: Rc<RefCell<Box<dyn Mapper>>>,
    ) -> Self {
        CpuBus {
            ram: Ram2KB::default(),
            io_regsiters,
            ppu_registers,
            mapper,
            intr: Cell::default(),
            nmi_state: NmiState::default(),
            irq_line: Cell::default(),
        }
    }

    pub fn poll_interrupts(&self) {
        // poll NMI line with side effects
        if self.ppu_registers.nmi() {
            self.nmi_state.set_nmi_line();
        } else {
            self.nmi_state.clear_nmi_line();
        }

        if self.nmi_state.poll_nmi_state() {
            self.intr.set(Some(Interrupt::Nmi));
        } else if self.irq_line.get() {
            self.intr.set(Some(Interrupt::Irq));
        }
    }

    pub fn intr_latch(&self) -> Option<Interrupt> {
        self.intr.take()
    }

    pub fn set_irq_line(&self, state: bool) {
        self.irq_line.set(state); // TODO: handle more than one IRQ
    }
}

impl crate::memory::AddressSpace for CpuBus {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2000..=0x2007 => self.ppu_registers.read_u8(addr), // PPU
            0x2008..=0x3FFF => self.ppu_registers.read_u8(0x2000 + (addr % 8)), // PPU mirror

            0x4000..=0x401F => self.io_regsiters.read_u8(addr),

            0x4020..=0xFFFF => self.mapper.borrow().read_u8(addr), // PRG ROM/RAM and mapper
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x0800, value),

            0x2000..=0x2007 => self.ppu_registers.write_u8(addr, value), // PPU
            0x2008..=0x3FFF => self.ppu_registers.write_u8(0x2000 + (addr % 8), value), // PPU mirror

            0x4000..=0x401F => self.io_regsiters.write_u8(addr, value),

            0x4020..=0xFFFF => self.mapper.borrow().write_u8(addr, value), // PRG ROM/RAM and mapper
        }
    }
}
