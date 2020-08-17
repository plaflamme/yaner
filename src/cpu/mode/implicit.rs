use super::*;

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
pub(in crate::cpu) fn run<'a, O: ImplicitOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
    move || {
        let _ = cpu.pc_read_u8() as u16;
        operation.operate(cpu);
        yield CpuCycle::Tick;
    }
}
