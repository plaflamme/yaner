use super::*;
use crate::{memory::AddressSpace, memory_read};

//  #   address  R/W description
// --- --------- --- ------------------------------------------
//  1     PC      R  fetch opcode, increment PC
//  2     PC      R  fetch pointer address low, increment PC
//  3     PC      R  fetch pointer address high, increment PC
//  4   pointer   R  fetch low address to latch
//  5  pointer+1* R  fetch PCH, copy latch to PCL
//
// Note: * The PCH will always be fetched from the same page
//         than PCL, i.e. page boundary crossing is not handled.
pub(in crate::cpu) fn jmp(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    move || {
        let addr_lo = memory_read! { cpu.next_pc_read_u8() };

        let addr_hi = memory_read! { cpu.next_pc_read_u8() };

        let addr = (addr_hi as u16) << 8 | addr_lo as u16;
        let pc_lo = memory_read! { cpu.bus.read_u8(addr) };

        let addr = (addr_hi as u16) << 8 | addr_lo.wrapping_add(1) as u16;
        let pc_hi = memory_read! { cpu.bus.read_u8(addr) };
        cpu.pc.set((pc_hi as u16) << 8 | pc_lo as u16);

        OpTrace::Addr(addr)
    }
}
