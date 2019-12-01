use std::pin::Pin;
use crate::cartridge::Cartridge;
use crate::memory::Ram2KB;
use crate::cpu::{Cpu, CpuAddressSpace};
use crate::ppu::{Ppu, PpuAddressSpace};
use std::ops::{Generator, GeneratorState};
use std::fmt::Display;

pub struct Nes {
    ram: Ram2KB,
    cartridge: Cartridge,
    cpu: Cpu,
    ppu: Ppu,
    // TODO: apu
    // TODO: input
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Self {
        Nes {
            ram: Ram2KB::new(),
            cartridge,
            cpu: Cpu::new(),
            ppu: Ppu::new(),
        }
    }

    pub fn run(&self, start_at: Option<u16>) {
        let cpu_addr_space = CpuAddressSpace::new(&self.ram, &self.ppu, self.cartridge.mapper.as_addr_space());
        let ppu_addr_space = PpuAddressSpace::new(&self.ppu, self.cartridge.mapper.as_addr_space());

        let mut clock = 0u64;
        let mut cpu_clock = CpuClock::new();
        let mut ppu_clock = PpuClock::new();
        let mut cpu = self.cpu.run(&cpu_addr_space, start_at);
        let mut ppu = self.ppu.run(&ppu_addr_space);

        trace!("{} {} {}", self.cpu.write(&cpu_addr_space), ppu_clock, cpu_clock);
        loop {

            if clock % cpu_clock.divisor == 0 && !cpu_clock.suspended {
                match Pin::new(&mut cpu).resume() {
                    GeneratorState::Yielded(crate::cpu::CpuCycle::Tick) => cpu_clock.tick(),
                    GeneratorState::Yielded(crate::cpu::CpuCycle::OpComplete { pc: _, op: _, mode: _ }) => {
                        trace!("{} {} {}", self.cpu.write(&cpu_addr_space), ppu_clock, cpu_clock);
                        continue;
                    },
                    GeneratorState::Yielded(crate::cpu::CpuCycle::Halt) => {
                        trace!("HALT");
                        break;
                    },
                    GeneratorState::Complete(_) => unimplemented!()
                };
            }

            if clock % ppu_clock.divisor == 0 {
                match Pin::new(&mut ppu).resume() {
                    GeneratorState::Yielded(crate::ppu::PpuCycle::Tick) => ppu_clock.tick(),
                    GeneratorState::Complete(_) => unimplemented!()
                }
            }

            clock += 1;
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
        CpuClock { divisor: 12, cycle: 7, suspended: false }
    }
}

impl Clock for CpuClock {
    fn divisor(&self) -> u64 { 12 }

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
    fn divisor(&self) -> u64 { 4 }

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
