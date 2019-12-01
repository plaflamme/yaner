use crate::memory::AddressSpace;
use super::*;

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
pub(super) fn jmp<'a>(cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let addr_lo = cpu.pc_read_u8_next(mem_map);
        yield CpuCycle::Tick;

        let addr_hi = cpu.pc_read_u8_next(mem_map);
        yield CpuCycle::Tick;

        let addr = (addr_hi as u16) << 8 | addr_lo as u16;
        let pc_lo = mem_map.read_u8(addr);
        yield CpuCycle::Tick;

        let addr = (addr_hi as u16) << 8 | addr_lo.wrapping_add(1) as u16;
        let pc_hi = mem_map.read_u8(addr);
        cpu.pc.set((pc_hi as u16) << 8 | pc_lo as u16);
        yield CpuCycle::Tick;
    }
}
