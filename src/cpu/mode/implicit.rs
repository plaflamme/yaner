use crate::memory_read;

use super::*;

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
pub(in crate::cpu) fn run<'a, O: ImplicitOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    #[coroutine]
    || {
        memory_read! { cpu, cpu.pc_read_u8() };
        operation.operate(cpu);
        OpTrace::Implicit
    }
}
