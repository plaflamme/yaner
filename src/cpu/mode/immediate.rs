use super::*;

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch value, increment PC
pub(in crate::cpu) fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let value = cpu.next_pc_read_u8();
        operation.operate(cpu, value);
        yield CpuCycle::Tick;
    }
}
