use super::*;
use crate::memory_read;

//  #   address  R/W description
// --- --------- --- ---------------------------------------------
//  1     PC      R  fetch opcode, increment PC
//  2     PC      R  fetch operand, increment PC
//  3     PC      R  Fetch opcode of next instruction,
//                   If branch is taken, add operand to PCL.
//                   Otherwise increment PC.
//  4+    PC*     R  Fetch opcode of next instruction.
//                   Fix PCH. If it did not change, increment PC.
//  5!    PC      R  Fetch opcode of next instruction,
//                   increment PC.
//
// Notes: The opcode fetch of the next instruction is included to
//        this diagram for illustration purposes. When determining
//        real execution times, remember to subtract the last
//        cycle.
//
//        * The high byte of Program Counter (PCH) may be invalid
//          at this time, i.e. it may be smaller or bigger by $100.
//
//        + If branch is taken, this cycle will be executed.
//
//        ! If branch occurs to different page, this cycle will be
//          executed.
pub(in crate::cpu) fn branch<'a, O: BranchOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let operand = memory_read! { cpu.next_pc_read_u8() as i8 };

        if !operation.branch(cpu) {
            OpTrace::Implicit
        } else {
            let pc = cpu.pc.get();
            let addr = (pc as i16).wrapping_add(operand as i16) as u16;

            memory_read! { cpu.bus.read_u8(pc) };

            if (pc & 0xFF00) != (addr & 0xFF00) {
                // crossing page boundary incurs an additional cycle
                memory_read! { cpu.bus.read_u8(pc) };
            }

            cpu.pc.set(addr);
            OpTrace::Addr(addr)
        }
    }
}
