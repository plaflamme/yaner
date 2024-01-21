use crate::cpu::Cpu;
use crate::memory::AddressSpace;
use crate::{memory_read, memory_write};
use std::ops::Coroutine;

use super::CpuCycle;

pub fn run(
    cpu: &Cpu,
    addr: u16,
    require_alignment: bool, // TODO: not exactly sure what this is.
) -> impl Coroutine<Yield = CpuCycle, Return = ()> + '_ {
    move || {
        // http://wiki.nesdev.com/w/index.php/PPU_OAM#DMA

        // dummy read cycle
        memory_read!(cpu, ());
        if require_alignment {
            // extra cycle on odd cpu cycles
            memory_read!(cpu, ());
        }

        for addr_lo in 0u16..=0xFF {
            let cpu_addr = addr | addr_lo;
            let value = memory_read! { cpu, cpu.bus.read_u8(cpu_addr) };

            // 0x2004 is OAMDATA
            memory_write! { cpu, cpu.bus.write_u8(0x2004, value) }
            if addr_lo == 0xFF {
                break;
            }
        }
    }
}
