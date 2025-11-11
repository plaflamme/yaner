use super::*;
use crate::memory::AddressSpace;
use crate::{memory_read, memory_write};

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch address, increment PC
//  3  address  R  read from effective address
pub(in crate::cpu) fn read<'a, O: ReadOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    #[coroutine]
    move || {
        let addr = memory_read! { cpu, cpu.next_pc_read_u8() as u16 };

        let value = memory_read! { cpu, cpu.bus.read_u8(addr) };
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
    #[coroutine]
    move || {
        let addr = memory_read! { cpu, cpu.next_pc_read_u8() as u16};

        let value = memory_read! { cpu, cpu.bus.read_u8(addr) };

        memory_write! { cpu, cpu.bus.write_u8(addr, value) };
        let (_, result) = operation.modify(cpu, addr, value);

        memory_write! { cpu, cpu.bus.write_u8(addr, result) };
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
    #[coroutine]
    move || {
        let addr = memory_read! { cpu, cpu.next_pc_read_u8() as u16};

        memory_write! { cpu, cpu.bus.write_u8(addr, operation.operate(cpu)) };
        OpTrace::Addr(addr)
    }
}
