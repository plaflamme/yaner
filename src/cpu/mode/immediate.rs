use crate::memory_read;

use super::*;

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch value, increment PC
pub(in crate::cpu) fn read<'a, O: ReadOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let value = memory_read! { cpu, cpu.next_pc_read_u8() };
        operation.operate(cpu, value);
        OpTrace::Implicit
    }
}
