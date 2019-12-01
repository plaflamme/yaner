use std::pin::Pin;
use crate::cartridge::Cartridge;
use crate::memory::Ram2KB;
use crate::cpu::{Cpu, CpuAddressSpace};
use crate::ppu::{Ppu, PpuAddressSpace};
use std::ops::{Generator, GeneratorState};

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

        let mut cycle = 7u32; // start at 7 due to reset interrupt handling
        let mut cpu_cycles = self.cpu.run(&cpu_addr_space, start_at);
        let mut ppu_cycles = self.ppu.run(&ppu_addr_space);

        trace!("{} CYC:{}", self.cpu.write(&cpu_addr_space), cycle);
        loop {
            match Pin::new(&mut cpu_cycles).resume() {
                GeneratorState::Yielded(crate::cpu::CpuCycle::Tick) => cycle = cycle + 1,
                GeneratorState::Yielded(crate::cpu::CpuCycle::OpComplete { pc: _, op: _, mode: _ }) => {
                    trace!("{} CYC:{}", self.cpu.write(&cpu_addr_space), cycle);
                },
                GeneratorState::Yielded(crate::cpu::CpuCycle::Halt) => {
                    trace!("HALT");
                    break;
                },
                GeneratorState::Complete(_) => unimplemented!()
            };

            for _ in 0..3 {
                match Pin::new(&mut ppu_cycles).resume() {
                    GeneratorState::Yielded(crate::ppu::PpuCycle::Tick) => (),
                    GeneratorState::Complete(_) => unimplemented!()
                };
            }
        }
    }
}
