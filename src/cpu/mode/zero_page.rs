use super::*;
use crate::memory::AddressSpace;

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch address, increment PC
//  3  address  R  read from effective address
pub(in crate::cpu) fn read<'a, O: ReadOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = cpu.next_pc_read_u8() as u16;
        yield CpuCycle::Tick;

        let value = cpu.bus.read_u8(addr);
        operation.operate(cpu, value);
        OpTrace::Addr(addr)
    }
}

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch address, increment PC
//  3  address  R  read from effective address
//  4  address  W  write the value back to effective address,
//                 and do the operation on it
//  5  address  W  write the new value to effective address
pub(in crate::cpu) fn modify<'a, O: ModifyOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = cpu.next_pc_read_u8() as u16;
        yield CpuCycle::Tick;

        let value = cpu.bus.read_u8(addr);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        let (_, result) = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, result);
        OpTrace::Addr(addr)
    }
}

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch address, increment PC
//  3  address  W  write register to effective address
pub(in crate::cpu) fn write<'a, O: WriteOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = cpu.next_pc_read_u8() as u16;
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, operation.operate(cpu));
        OpTrace::Addr(addr)
    }
}
