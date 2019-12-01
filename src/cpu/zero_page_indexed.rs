use crate::memory::AddressSpace;
use super::*;

fn zp_indexed<'a>(index: u8, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = (u16, u8)> + 'a {
    move || {
        let addr = cpu.pc_read_u8_next(mem_map);
        yield CpuCycle::Tick;

        let _ = mem_map.read_u8(addr as u16);
        let addr = addr.wrapping_add(index) as u16;
        yield CpuCycle::Tick;

        let value = mem_map.read_u8(addr);
        (addr, value)
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
fn read<'a, O: ReadOperation>(operation: &'a O, index: u8, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (_, value) = yield_complete!(zp_indexed(index, cpu, mem_map));
        operation.operate(cpu, value);
        yield CpuCycle::Tick;
    }
}

pub(super) fn x_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    read(operation, cpu.x.get(), cpu, mem_map)
}

pub(super) fn y_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    read(operation, cpu.y.get(), cpu, mem_map)
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
fn modify<'a, O: ModifyOperation>(operation: &'a O, index: u8, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, value) = yield_complete!(zp_indexed(index, cpu, mem_map));
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, value);
        let value = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, value);
        yield CpuCycle::Tick;
    }
}

pub(super) fn x_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    modify(operation, cpu.x.get(), cpu, mem_map)
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
fn write<'a, O: WriteOperation>(operation: &'a O, index: u8, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, _) = yield_complete!(zp_indexed(index, cpu, mem_map));
        let value = operation.operate(cpu);
        mem_map.write_u8(addr, value);
        yield CpuCycle::Tick;
    }
}

pub(super) fn x_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    write(operation, cpu.x.get(), cpu, mem_map)
}

pub(super) fn y_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    write(operation, cpu.y.get(), cpu, mem_map)
}
