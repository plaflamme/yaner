use std::pin::Pin;
use std::ops::{Generator, GeneratorState};
use std::fmt::{Display, Error, Formatter};

use dma::DmaCycle;
use crate::cartridge::Cartridge;
use crate::cpu::{Cpu, CpuBus, CpuCycle};
use crate::cpu::opcode::OpCode;
use crate::ppu::{Ppu, PpuBus, PpuCycle, MemoryMappedRegisters};
use std::rc::Rc;
use std::cell::RefCell;
use crate::nes::dma::Dma;
use crate::memory::AddressSpace;

mod dma;

pub enum NesCycle {
    CpuOp(u64, OpCode),
    PpuFrame,
}

pub struct Nes {
    cpu: Cpu,
    ppu: Rc<Ppu>,
    dma: Dma,
    // TODO: apu
    // TODO: input
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        let ppu = Rc::new(Ppu::new());
        let mapper = Rc::new(RefCell::new(cartridge.mapper));
        let ppu_bus = PpuBus::new(mapper.clone());
        let ppu_mem_registers = MemoryMappedRegisters::new(ppu.clone(), ppu_bus);
        let cpu_bus = CpuBus::new(ppu_mem_registers, mapper.clone());

        Nes {
            cpu: Cpu::new(cpu_bus),
            ppu,
            dma: Dma::new(),
        }
    }

    pub fn ram(&self) -> &dyn AddressSpace {
        &self.cpu.bus
    }

    pub fn run<'a>(&'a self, start_at: Option<u16>) -> impl Generator<Yield = NesCycle, Return = ()> + 'a {
        move || {

            let mut clock = 0u64;
            let mut cpu_clock = CpuClock::new();
            let mut ppu_clock = PpuClock::new();
            let mut cpu = self.cpu.run(start_at);
            let mut ppu = self.ppu.run();
            let mut dma = self.dma.run(&self.cpu.bus);

            trace!("{} {} {}", self.cpu, ppu_clock, cpu_clock);
            loop {
                if clock % cpu_clock.divisor == 0 && !cpu_clock.suspended {
                    match Pin::new(&mut cpu).resume(()) {
                        GeneratorState::Yielded(CpuCycle::Tick) => cpu_clock.tick(),
                        GeneratorState::Yielded(CpuCycle::OpComplete(opcode)) => {
                            trace!("{} {} {}", self.cpu, ppu_clock, cpu_clock);
                            yield NesCycle::CpuOp(clock, opcode);
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
                    self.dma.start(addr, cpu_clock.cycle);
                    cpu_clock.suspend();
                }

                if clock % ppu_clock.divisor == 0 {
                    match Pin::new(&mut ppu).resume(()) {
                        GeneratorState::Yielded(PpuCycle::Tick) => ppu_clock.tick(),
                        GeneratorState::Yielded(PpuCycle::Frame) => {
                            yield NesCycle::PpuFrame;
                            continue;
                        },
                        GeneratorState::Complete(_) => unimplemented!()
                    }
                }

                // go as fast as the PPU
                clock = clock.wrapping_add(ppu_clock.divisor);

                if clock % 10_000 == 0 {
                    self.ppu.decay_open_bus()
                }
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
