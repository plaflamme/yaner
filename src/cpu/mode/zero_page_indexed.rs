use crate::memory::AddressSpace;
use super::*;

fn zp_indexed<'a>(index: u8, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = u16> + 'a {
    move || {
        let addr = cpu.next_pc_read_u8();
        yield CpuCycle::Tick;

        let _ = cpu.bus.read_u8(addr as u16);
        let addr = addr.wrapping_add(index) as u16;
        yield CpuCycle::Tick;

        addr
    }
}

//  #   address  R/W description
// --- --------- --- ------------------------------------------
//  1     PC      R  fetch opcode, increment PC
//  2     PC      R  fetch address, increment PC
//  3   address   R  read from address, add index register to it
//  4  address+I* R  read from effective address
//  Notes: I denotes either index register (X or Y).
//
//        * The high byte of the effective address is always zero,
//          i.e. page boundary crossings are not handled.
fn read<'a, O: ReadOperation>(operation: &'a O, index: u8, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(zp_indexed(index, cpu));
        let value = cpu.bus.read_u8(addr);
        operation.operate(cpu, value);
        OpTrace::Addr(addr)
    }
}

pub(in crate::cpu) fn x_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    read(operation, cpu.x.get(), cpu)
}

pub(in crate::cpu) fn y_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    read(operation, cpu.y.get(), cpu)
}

//  #   address  R/W description
// --- --------- --- ---------------------------------------------
//  1     PC      R  fetch opcode, increment PC
//  2     PC      R  fetch address, increment PC
//  3   address   R  read from address, add index register X to it
//  4  address+X* R  read from effective address
//  5  address+X* W  write the value back to effective address,
//                   and do the operation on it
//  6  address+X* W  write the new value to effective address
//
// Note: * The high byte of the effective address is always zero,
//         i.e. page boundary crossings are not handled.
fn modify<'a, O: ModifyOperation>(operation: &'a O, index: u8, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(zp_indexed(index, cpu));
        let value = cpu.bus.read_u8(addr);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        let (_, value) = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        OpTrace::Addr(addr)
    }
}

pub(in crate::cpu) fn x_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    modify(operation, cpu.x.get(), cpu)
}

//  #   address  R/W description
// --- --------- --- -------------------------------------------
//  1     PC      R  fetch opcode, increment PC
//  2     PC      R  fetch address, increment PC
//  3   address   R  read from address, add index register to it
//  4  address+I* W  write to effective address
//
// Notes: I denotes either index register (X or Y).
//
//        * The high byte of the effective address is always zero,
//          i.e. page boundary crossings are not handled.
fn write<'a, O: WriteOperation>(operation: &'a O, index: u8, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(zp_indexed(index, cpu));
        let value = operation.operate(cpu);
        cpu.bus.write_u8(addr, value);
        OpTrace::Addr(addr)
    }
}

pub(in crate::cpu) fn x_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    write(operation, cpu.x.get(), cpu)
}

pub(in crate::cpu) fn y_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    write(operation, cpu.y.get(), cpu)
}
