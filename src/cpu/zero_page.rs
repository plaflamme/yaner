use crate::memory::AddressSpace;
use super::*;

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch address, increment PC
//  3  address  R  read from effective address
pub(super) fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let addr = cpu.pc_read_u8_next(mem_map) as u16;
        yield CpuCycle::Tick;

        let value = mem_map.read_u8(addr);
        operation.operate(cpu, value);
        yield CpuCycle::Tick;
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
pub(super) fn modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let addr = cpu.pc_read_u8_next(mem_map) as u16;
        yield CpuCycle::Tick;

        let value = mem_map.read_u8(addr);
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, value);
        let result = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, result);
        yield CpuCycle::Tick;
    }
}

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch address, increment PC
//  3  address  W  write register to effective address
pub(super) fn write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let addr = cpu.pc_read_u8_next(mem_map) as u16;
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, operation.operate(cpu));
        yield CpuCycle::Tick;
    }
}
