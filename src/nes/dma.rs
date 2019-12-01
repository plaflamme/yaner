use crate::memory::AddressSpace;
use std::ops::Generator;
use std::cell::Cell;

pub enum DmaCycle {
    Tick,
    Done,
    NoDma
}

#[derive(Clone, Copy)]
struct DmaState {
    addr_hi: u16,
    odd_cycle: bool,
}

impl DmaState {
    fn new(addr: u8, odd_cycle: bool) -> Self {
        DmaState { addr_hi: (addr as u16) << 8, odd_cycle }
    }
}

pub struct Dma {
    state: Cell<Option<DmaState>>
}

impl Dma {
    pub fn new() -> Self {
        Dma { state: Cell::new(None) }
    }

    pub fn start(&self, addr: u8, cpu_cycle: u64) {
        let state = DmaState::new(addr, cpu_cycle % 2 == 1);
        self.state.set(Some(state));
    }

    pub fn run<'a>(&'a self, cpu: &'a dyn AddressSpace) -> impl Generator<Yield=DmaCycle, Return=()> + 'a {
        move || {
            loop {
                match self.state.get() {
                    None => yield DmaCycle::NoDma,
                    Some(state) => {
                        for addr_lo in 0u16..=0xFF {
                            let cpu_addr = state.addr_hi | addr_lo;
                            let value = cpu.read_u8(cpu_addr);
                            yield DmaCycle::Tick;

                            // 0x2004 is OAMDATA
                            cpu.write_u8(0x2004, value);
                            yield DmaCycle::Tick;
                        }
                        // dummy read cycle
                        yield DmaCycle::Tick;

                        if state.odd_cycle {
                            // extra cycle on odd cpu cycles
                            yield DmaCycle::Tick;
                        }

                        yield DmaCycle::Done;
                    }
                }
            }
        }
    }
}
