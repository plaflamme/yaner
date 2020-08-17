use std::pin::Pin;
use std::ops::{Generator, GeneratorState};
use std::fmt::{Display, Error, Formatter};

use dma::DmaCycle;
use crate::cartridge::Cartridge;
use crate::memory::AddressSpace;
use crate::cpu::{Cpu, CpuAddressSpace, CpuCycle};
use crate::ppu::{Ppu, PpuAddressSpace, PpuCycle, MemoryMappedRegisters};
use std::rc::Rc;
use std::cell::RefCell;

mod dma;

pub struct Nes {
    // cartridge: Cartridge,
    cpu: Cpu,
    // ppu: Ppu,
    // TODO: apu
    // TODO: input
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        let mapper = Rc::new(RefCell::new(cartridge.mapper));
        let ppu_addr_space = PpuAddressSpace::new(mapper.clone());
        let ppu_mem_registers = MemoryMappedRegisters::new(Ppu::new(), ppu_addr_space);
        let cpu_addr_space = CpuAddressSpace::new(ppu_mem_registers, mapper.clone());

        Nes {
            // cartridge,
            cpu: Cpu::new(cpu_addr_space),
            // ppu,
        }
    }

    pub fn run(&self, start_at: Option<u16>, mut halt: impl FnMut(&dyn AddressSpace) -> bool) {
        let oam_dma = dma::Dma::new();

        let mut clock = 0u64;
        let mut cpu_clock = CpuClock::new();
        let mut ppu_clock = PpuClock::new();
        let mut cpu = self.cpu.run(start_at);
        let mut ppu = self.cpu.bus.ppu.ppu.run();
        let mut dma = oam_dma.run(&self.cpu.bus);

        trace!("{} {} {}", self.cpu, ppu_clock, cpu_clock);
        loop {

            if clock % cpu_clock.divisor == 0 && !cpu_clock.suspended {
                match Pin::new(&mut cpu).resume(()) {
                    GeneratorState::Yielded(CpuCycle::Tick) => cpu_clock.tick(),
                    GeneratorState::Yielded(CpuCycle::OpComplete) => {
                        trace!("{} {} {}", self.cpu, ppu_clock, cpu_clock);
                        continue;
                    },
                    GeneratorState::Yielded(CpuCycle::Halt) => {
                        trace!("HALT");
                        break;
                    },
                    GeneratorState::Complete(_) => unimplemented!()
                };
            }

            match Pin::new(&mut dma).resume(()) {
                GeneratorState::Yielded(DmaCycle::NoDma) => (),
                GeneratorState::Yielded(DmaCycle::Tick) => (),
                GeneratorState::Yielded(DmaCycle::Done) => cpu_clock.resume(),
                GeneratorState::Complete(_) => (),
            };

            if let Some(addr) = self.cpu.bus.dma_latch() {
                oam_dma.start(addr, cpu_clock.cycle);
                cpu_clock.suspend();
            }

            if clock % ppu_clock.divisor == 0 {
                match Pin::new(&mut ppu).resume(()) {
                    GeneratorState::Yielded(PpuCycle::Tick) => ppu_clock.tick(),
                    GeneratorState::Complete(_) => unimplemented!()
                }
            }

            clock += 1;

            if clock % 10_000 == 0 {
                self.cpu.bus.ppu.ppu.decay_open_bus()
            }

            if halt(&self.cpu.bus) {
                break;
            }
        }
    }
}

trait Clock {
    fn divisor(&self) -> u64;
    fn tick(&mut self);
    fn suspended(&self) -> bool;
}

struct CpuClock {
    divisor: u64,
    cycle: u64,
    suspended: bool
}

impl CpuClock {
    fn new() -> Self {
        // start at 7 due to reset interrupt handling
        //   See start sequence here http://users.telenet.be/kim1-6502/6502/proman.html#92
        CpuClock { divisor: 12, cycle: 7, suspended: false }
    }

    fn suspend(&mut self) {
        self.suspended = true;
    }

    fn resume(&mut self) {
        self.suspended = false;
    }
}

impl Clock for CpuClock {
    fn divisor(&self) -> u64 { self.divisor }

    fn tick(&mut self) {
        self.cycle += 1;
    }

    fn suspended(&self) -> bool {
        self.suspended
    }
}

impl Display for CpuClock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "CYC:{}", self.cycle)
    }
}

struct PpuClock {
    divisor: u64,
    cycle: u64
}

impl PpuClock {
    fn new() -> Self {
        PpuClock { divisor: 4, cycle: 0 }
    }
}

impl Clock for PpuClock {
    fn divisor(&self) -> u64 { self.divisor }

    fn tick(&mut self) {
        self.cycle += 1;
    }

    fn suspended(&self) -> bool { false }
}

impl Display for PpuClock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "PPU:{}, {}", self.cycle % 341, self.cycle / 341)
    }
}
