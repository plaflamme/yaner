use super::*;

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch value, increment PC
pub(in crate::cpu) fn read<O: ReadOperation>(operation: &O, cpu: &Cpu) -> OpTrace {
    let value = cpu.next_pc_read_u8();
    operation.operate(cpu, value);
    OpTrace::Implicit
}
