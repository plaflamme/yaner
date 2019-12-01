use super::*;

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch value, increment PC
pub(super) fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let value = cpu.pc_read_u8_next(mem_map);
        operation.operate(cpu, value);
        yield CpuCycle::Tick;
    }
}
