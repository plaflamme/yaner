use std::cell::{Cell, RefCell};
use std::fmt::{Display, Error, Formatter};
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::rc::Rc;

use dma::DmaCycle;

use crate::cartridge::Cartridge;
use crate::cpu::{Cpu, CpuBus, CpuCycle};
use crate::memory::AddressSpace;
use crate::nes::dma::Dma;
use crate::ppu::{MemoryMappedRegisters, Ppu, PpuBus};

mod dma;

pub enum NesCycle {
    PowerUp,
    Tick(CpuCycle),
}

pub struct Nes {
    pub cpu: Cpu,
    ppu: Rc<Ppu>,
    dma: Dma,
    pub cpu_clock: CpuClock,
    pub ppu_clock: PpuClock,
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
            cpu_clock: CpuClock::new(),
            ppu_clock: PpuClock::new(),
        }
    }

    pub fn ram(&self) -> &dyn AddressSpace {
        &self.cpu.bus
    }

    pub fn run<'a>(&'a self, start_at: Option<u16>) -> impl Generator<Yield = NesCycle, Return = ()> + 'a {

        let mut clock = 0u64;
        let mut cpu = self.cpu.run(start_at);
        let mut ppu = self.ppu.run();
        let mut dma = self.dma.run(&self.cpu.bus);

        move || {

            // TODO: this steps according to how nestest assumes things
            //   but we can implement different styles, e.g.: step per frame, per cpu op, per nes tick, etc.
            trace!("{} {} {}", self.cpu, self.ppu_clock, self.cpu_clock);
            yield NesCycle::PowerUp;
            loop {

                let mut cpu_step = || {
                    match Pin::new(&mut cpu).resume(()) {
                        GeneratorState::Yielded(cycle) => {
                            self.cpu_clock.tick();
                            cycle
                        },
                        GeneratorState::Complete(_) => panic!("cpu stopped"),
                    }
                };

                let mut ppu_cycle = || {
                    match Pin::new(&mut ppu).resume(()) {
                        GeneratorState::Yielded(ppu_cycle) => {
                            self.ppu_clock.tick();
                            ppu_cycle
                        },
                        GeneratorState::Complete(_) => panic!("ppu stopped")
                    }
                };

                let cpu_cycle = cpu_step();

                match Pin::new(&mut dma).resume(()) {
                    GeneratorState::Yielded(DmaCycle::NoDma) => (),
                    GeneratorState::Yielded(DmaCycle::Tick) => (),
                    GeneratorState::Yielded(DmaCycle::Done) => self.cpu_clock.resume(),
                    GeneratorState::Complete(_) => (),
                };

                if let Some(addr) = self.cpu.bus.dma_latch() {
                    self.dma.start(addr, self.cpu_clock.cycle.get());
                    self.cpu_clock.suspend();
                }

                ppu_cycle();
                ppu_cycle();
                ppu_cycle();

                match cpu_cycle {
                    CpuCycle::OpComplete(opcode, optrace) => {
                        trace!("{:?} {:?}", opcode, optrace);
                        trace!("{} {} {}", self.cpu, self.ppu_clock, self.cpu_clock);
                        yield NesCycle::Tick(cpu_cycle);
                    },
                    CpuCycle::Halt => break,
                    _ => ()
                };

                // go as fast as the PPU
                clock = clock.wrapping_add(self.ppu_clock.divisor);

                if clock % 10_000 == 0 {
                    self.ppu.decay_open_bus()
                }
            }
        }
    }
}

trait Clock {
    fn divisor(&self) -> u64;
    fn tick(&self);
    fn suspended(&self) -> bool;
}

pub struct CpuClock {
    divisor: u64,
    cycle: Cell<u64>,
    suspended: Cell<bool>
}

impl CpuClock {
    fn new() -> Self {
        // start at 7 due to reset interrupt handling
        //   See start sequence here http://users.telenet.be/kim1-6502/6502/proman.html#92
        CpuClock { divisor: 12, cycle: Cell::new(7), suspended: Cell::new(false) }
    }

    pub fn cycle(&self) -> u64 {
        self.cycle.get()
    }

    fn suspend(&self) {
        self.suspended.set(true);
    }

    fn resume(&self) {
        self.suspended.set(false);
    }
}

impl Clock for CpuClock {
    fn divisor(&self) -> u64 { self.divisor }

    fn tick(&self) {
        self.cycle.update(|c| c + 1);
    }

    fn suspended(&self) -> bool {
        self.suspended.get()
    }
}

impl Display for CpuClock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "CYC:{}", self.cycle.get())
    }
}

pub struct PpuClock {
    divisor: u64,
    cycle: Cell<u64>
}

impl PpuClock {
    fn new() -> Self {
        PpuClock { divisor: 4, cycle: Cell::new(21) }
    }

    pub fn frame(&self) -> u64 {
        self.cycle.get() / 341
    }

    pub fn dot(&self) -> u16 {
        (self.cycle.get() % 341) as u16
    }
}

impl Clock for PpuClock {
    fn divisor(&self) -> u64 { self.divisor }

    fn tick(&self) {
        self.cycle.update(|c| c + 1);
    }

    fn suspended(&self) -> bool { false }
}

impl Display for PpuClock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "PPU:{:>3},{:>3}", self.frame(), self.dot())
    }
}
