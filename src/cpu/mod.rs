#![allow(non_camel_case_types)]

use std::ops::{Coroutine, CoroutineState};
use yaner_cpu::Cpu;
use yaner_cpu::CpuEvent;

use crate::apu::Apu;
use crate::apu::ApuCycle;
use crate::memory::AddressSpace;
use std::cell::Cell;
use std::pin::Pin;

pub struct RP2A03<A> {
    pub(crate) clock: Cell<u64>,
    pub(crate) apu_clock: Cell<u64>, // TODO: remove this?
    pub(crate) cpu: Cpu,
    pub(crate) apu: Apu,
    pub(crate) bus: A,
    oam_latch: Cell<Option<u8>>,
}

impl<A: AddressSpace> RP2A03<A> {
    pub fn new(start_at: Option<u16>, bus: A) -> Self {
        Self {
            clock: Cell::new(12),
            apu_clock: Cell::default(),
            cpu: Cpu::new(start_at),
            apu: Apu::default(),
            oam_latch: Cell::default(),
            bus,
        }
    }

    pub(crate) fn set_nmi(&self, state: bool) {
        self.cpu.set_nmi(state);
    }

    pub(crate) fn active_pc(&self) -> u16 {
        self.cpu.active_pc()
    }

    pub(crate) fn reset(&self) {
        self.cpu.reset();
    }

    // Read value from addr and write it to the CPU IO bus.
    fn cpu_read_u8(&self, addr: u16) {
        self.cpu.io_bus.set(self.read_u8(addr));
    }

    // Write the value from the CPU's IO bus to addr.
    fn cpu_write_u8(&self, addr: u16) {
        self.write_u8(addr, self.cpu.io_bus.get());
    }

    pub fn run(&self) -> impl Coroutine<Yield = CpuEvent, Return = ()> + '_ {
        #[coroutine]
        || {
            let apu_stride = 12;
            let mut cpu = self.cpu.run();
            let mut apu = self.apu.run();
            macro_rules! tick_apu {
                () => {
                    while self.apu_clock.get() + apu_stride < self.clock.get() {
                        match Pin::new(&mut apu).resume(()) {
                            CoroutineState::Yielded(ApuCycle::Tick { irq }) => {
                                if irq {
                                    self.cpu.set_irq(irq);
                                }
                            }
                            CoroutineState::Complete(_) => (),
                        }
                        self.apu_clock.update(|c| c + apu_stride);
                    }
                };
            }
            macro_rules! read {
                ($addr:expr) => {
                    self.clock.update(|c| c + 5);
                    tick_apu!();
                    yield yaner_cpu::CpuEvent::HalfCycle {
                        phase: yaner_cpu::Phase::One,
                        rw: yaner_cpu::Rw::Read,
                        addr: $addr,
                    };
                    self.cpu_read_u8($addr);
                    self.clock.update(|c| c + 7);
                    tick_apu!();
                    yield yaner_cpu::CpuEvent::HalfCycle {
                        phase: yaner_cpu::Phase::Two,
                        rw: yaner_cpu::Rw::Read,
                        addr: $addr,
                    };
                };
            }
            macro_rules! write {
                ($addr:expr) => {
                    self.clock.update(|c| c + 7);
                    tick_apu!();
                    yield yaner_cpu::CpuEvent::HalfCycle {
                        phase: yaner_cpu::Phase::One,
                        rw: yaner_cpu::Rw::Write,
                        addr: $addr,
                    };
                    self.cpu_write_u8($addr);
                    self.clock.update(|c| c + 5);
                    tick_apu!();
                    yield yaner_cpu::CpuEvent::HalfCycle {
                        phase: yaner_cpu::Phase::Two,
                        rw: yaner_cpu::Rw::Write,
                        addr: $addr,
                    };
                };
            }
            loop {
                {
                    match Pin::new(&mut cpu).resume(()) {
                        CoroutineState::Yielded(
                            cycle @ yaner_cpu::CpuEvent::HalfCycle {
                                phase: yaner_cpu::Phase::One,
                                rw,
                                addr,
                            },
                        ) => match rw {
                            yaner_cpu::Rw::Read => {
                                if let Some(addr) = self.oam_latch.take() {
                                    let addr = (addr as u16) << 8;
                                    log::debug!("DMA start {addr:02X}");
                                    read!(addr);
                                    let cycles = self.clock.get() / 12;
                                    if cycles.is_multiple_of(2) {
                                        read!(addr);
                                    }
                                    for addr_lo in 0x00..=0xFF {
                                        read!(addr | addr_lo);
                                        write!(0x2004);
                                    }
                                }

                                self.clock.update(|c| c + 5);
                                tick_apu!();
                                yield cycle;
                                self.cpu_read_u8(addr);
                            }
                            yaner_cpu::Rw::Write => {
                                self.clock.update(|c| c + 7);
                                tick_apu!();
                                yield cycle;
                                self.cpu_write_u8(addr);
                            }
                        },
                        CoroutineState::Yielded(
                            cycle @ yaner_cpu::CpuEvent::HalfCycle {
                                phase: yaner_cpu::Phase::Two,
                                rw,
                                addr: _,
                            },
                        ) => match rw {
                            yaner_cpu::Rw::Read => {
                                self.clock.update(|c| c + 7);
                                tick_apu!();
                                yield cycle;
                            }
                            yaner_cpu::Rw::Write => {
                                self.clock.update(|c| c + 5);
                                tick_apu!();
                                yield cycle;
                            }
                        },

                        CoroutineState::Complete(_) => panic!("cpu stopped"),
                    };
                };
            }
        }
    }
}

impl<A: AddressSpace> crate::memory::AddressSpace for RP2A03<A> {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            // "IO registers"
            0x4000..=0x4014 => invalid_address!(addr, 0x00),
            0x4015 => self.apu.read_u8(addr),
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.
            _ => self.bus.read_u8(addr),
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            // "IO registers"
            0x4014 => self.oam_latch.set(Some(value)),
            0x4000..=0x4013 | 0x4015 | 0x4017 => self.apu.write_u8(addr, value),
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.
            _ => self.bus.write_u8(addr, value),
        }
    }
}
