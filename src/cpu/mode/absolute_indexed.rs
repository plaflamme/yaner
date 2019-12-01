use crate::memory::AddressSpace;
use super::*;

fn abs_indexed<'a>(index: u8, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = (u16, u8, bool)> + 'a {
    move || {
        let addr_lo = cpu.pc_read_u8_next(mem_map);
        yield CpuCycle::Tick;

        let addr_hi = cpu.pc_read_u8_next(mem_map) as u16;
        let addr_pre = (addr_hi << 8) | addr_lo.wrapping_add(index) as u16;
        yield CpuCycle::Tick;

        let _ = mem_map.read_u8(addr_pre);
        let addr = (addr_hi << 8 | addr_lo as u16).wrapping_add(index as u16);

        let value = mem_map.read_u8(addr);
        (addr, value, addr != addr_pre)
    }
}

//  #   address  R/W description
// --- --------- --- ------------------------------------------
//  1     PC      R  fetch opcode, increment PC
//  2     PC      R  fetch low byte of address, increment PC
//  3     PC      R  fetch high byte of address,
//                   add index register to low address byte,
//                   increment PC
//  4  address+I* R  read from effective address,
//                   fix the high byte of effective address
//  5+ address+I  R  re-read from effective address
//
//  Notes: I denotes either index register (X or Y).
//
//        * The high byte of the effective address may be invalid
//          at this time, i.e. it may be smaller by $100.
//
//        + This cycle will be executed only if the effective address
//          was invalid during cycle #4, i.e. page boundary was crossed.
fn read<'a, O: ReadOperation>(operation: &'a O, index: u8, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (_, value, oops) = yield_complete!(abs_indexed(index, cpu, mem_map));
        if oops {
            yield CpuCycle::Tick;
        }
        operation.operate(cpu, value);
        yield CpuCycle::Tick;
    }
}

pub(in crate::cpu) fn x_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    read(operation, cpu.x.get(), cpu, mem_map)
}

pub(in crate::cpu) fn y_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    read(operation, cpu.y.get(), cpu, mem_map)
}

//  #   address  R/W description
// --- --------- --- ------------------------------------------
//  1    PC       R  fetch opcode, increment PC
//  2    PC       R  fetch low byte of address, increment PC
//  3    PC       R  fetch high byte of address,
//                   add index register X to low address byte,
//                   increment PC
//  4  address+X* R  read from effective address,
//                   fix the high byte of effective address
//  5  address+X  R  re-read from effective address
//  6  address+X  W  write the value back to effective address,
//                   and do the operation on it
//  7  address+X  W  write the new value to effective address
//
// Notes: * The high byte of the effective address may be invalid
//          at this time, i.e. it may be smaller by $100.
fn modify<'a, O: ModifyOperation>(operation: &'a O, index: u8, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, value, _) = yield_complete!(abs_indexed(index, cpu, mem_map));
        yield CpuCycle::Tick;

        mem_map.read_u8(addr);
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, value);
        let value = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, value);
        yield CpuCycle::Tick;
    }
}

pub(in crate::cpu) fn x_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    modify(operation, cpu.x.get(), cpu, mem_map)
}

#[allow(dead_code)]
pub(in crate::cpu) fn y_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    modify(operation, cpu.y.get(), cpu, mem_map)
}

//  #   address  R/W description
// --- --------- --- ------------------------------------------
//  1     PC      R  fetch opcode, increment PC
//  2     PC      R  fetch low byte of address, increment PC
//  3     PC      R  fetch high byte of address,
//                   add index register to low address byte,
//                   increment PC
//  4  address+I* R  read from effective address,
//                   fix the high byte of effective address
//  5  address+I  W  write to effective address
//
// Notes: I denotes either index register (X or Y).
//
//        * The high byte of the effective address may be invalid
//          at this time, i.e. it may be smaller by $100. Because
//          the processor cannot undo a write to an invalid
//          address, it always reads from the address first.
fn write<'a, O: WriteOperation>(operation: &'a O, index: u8, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, _, _) = yield_complete!(abs_indexed(index, cpu, mem_map));
        yield CpuCycle::Tick;

        let value = operation.operate(cpu);
        mem_map.write_u8(addr, value);
        yield CpuCycle::Tick;
    }
}

pub(in crate::cpu) fn x_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    write(operation, cpu.x.get(), cpu, mem_map)
}

pub(in crate::cpu) fn y_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a dyn AddressSpace) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    write(operation, cpu.y.get(), cpu, mem_map)
}
