use crate::cpu::Cpu;
use crate::memory::AddressSpace;
use crate::{cpu_read_cycle, cpu_write_cycle};
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
        cpu_read_cycle!(());

        if require_alignment {
            // extra cycle on odd cpu cycles
            cpu_read_cycle!(());
        }

        for addr_lo in 0x00..=0xFF {
            let value = cpu_read_cycle!(cpu.bus.read_u8(addr | addr_lo));

            // 0x2004 is OAMDATA
            cpu_write_cycle!(cpu.bus.write_u8(0x2004, value));
            if addr_lo == 0xFF {
                break;
            }
        }
    }
}
