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
use crate::ppu::{MemoryMappedRegisters, Ppu, PpuBus, PpuCycle};

mod dma;

pub enum NesCycle {
    PowerUp,
    PpuCycle(PpuCycle),
    CpuCycle(CpuCycle, PpuCycle)
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

    // yields on every nes ppu tick
    pub fn ppu_steps(&self, start_at: Option<u16>) -> impl Generator<Yield = NesCycle, Return = ()> + '_ {

        let mut clock = 0u64;
        let mut cpu = self.cpu.run(start_at);
        let mut ppu = self.ppu.run();
        let mut dma = self.dma.run(&self.cpu.bus);

        move || {
            yield NesCycle::PowerUp;
            loop {

                let mut ppu_step = || {
                    match Pin::new(&mut ppu).resume(()) {
                        GeneratorState::Yielded(cycle) => {
                            self.ppu_clock.tick();
                            cycle
                        },
                        GeneratorState::Complete(_) => panic!("ppu stopped"),
                    }
                };

                let mut dma_step = || {
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
                };

                if !self.cpu_clock.suspended() && clock % self.cpu_clock.divisor == 0 {
                    match Pin::new(&mut cpu).resume(()) {
                        GeneratorState::Yielded(CpuCycle::Halt) => break,
                        GeneratorState::Yielded(cpu_cycle) => {
                            self.cpu_clock.tick();
                            yield NesCycle::CpuCycle(cpu_cycle, ppu_step())
                        },
                        GeneratorState::Complete(_) => panic!("cpu stopped"),
                    }
                } else {
                    if self.cpu_clock.suspended() {
                        dma_step();
                    }
                    yield NesCycle::PpuCycle(ppu_step());
                }

                // go as fast as the PPU
                clock = clock.wrapping_add(self.ppu_clock.divisor);

                if clock % 10_000 == 0 {
                    self.ppu.decay_open_bus()
                }
            }
        }
    }

    // yields only when the cpu finishes an operation
    //   note that this will hide ppu frames, so it's likely only useful for debugging
    pub fn cpu_steps(&self, start_at: Option<u16>) -> impl Generator<Yield = NesCycle, Return = ()> + '_ {

        let mut ppu_steps = self.ppu_steps(start_at);

        move || {
            loop {
                match Pin::new(&mut ppu_steps).resume(()) {
                    GeneratorState::Yielded(NesCycle::PowerUp) => {
                        trace!("{}", self);
                        yield NesCycle::PowerUp
                    },
                    GeneratorState::Yielded(cycle@NesCycle::CpuCycle(CpuCycle::OpComplete(_, _), _)) => {
                        trace!("{}", self);
                        yield cycle
                    },
                    GeneratorState::Yielded(_) => (),
                    GeneratorState::Complete(_) => break
                }
            }
        }
    }

    // yields once for every ppu frame
    pub fn ppu_frames(&self, start_at: Option<u16>) -> impl Generator<Yield = PpuCycle, Return = ()> + '_ {
        let mut ppu_steps = self.ppu_steps(start_at);
        move || {
            loop {
                match Pin::new(&mut ppu_steps).resume(()) {
                    GeneratorState::Yielded(NesCycle::CpuCycle(_, frame@PpuCycle::Frame)) => {
                        yield frame;
                    },
                    GeneratorState::Yielded(NesCycle::PpuCycle(frame@PpuCycle::Frame)) => {
                        yield frame;
                    },
                    GeneratorState::Yielded(_) => (),
                    GeneratorState::Complete(_) => break
                }
            }
        }
    }

    // runs the program until the CPU halts
    pub fn run(&self, start_at: Option<u16>) {
        consume_generator!(self.ppu_frames(start_at), ())
    }
}

impl Display for Nes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.cpu, self.ppu_clock, self.cpu_clock)
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
