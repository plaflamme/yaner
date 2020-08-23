use crate::memory::AddressSpace;
use super::*;

fn abs_indexed<'a>(index: u8, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = (u16, u16, u8, bool)> + 'a {
    move || {
        let addr_lo = cpu.next_pc_read_u8();
        yield CpuCycle::Tick;

        let addr_hi = (cpu.next_pc_read_u8() as u16) << 8;
        let addr_pre = addr_hi | (addr_lo.wrapping_add(index) as u16);
        yield CpuCycle::Tick;

        let value = cpu.bus.read_u8(addr_pre);
        let addr_fixed = (addr_hi | addr_lo as u16).wrapping_add(index as u16);

        let oops = addr_pre != addr_fixed;
        (addr_fixed, addr_pre, value, oops)
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
fn read<'a, O: ReadOperation>(operation: &'a O, index: u8, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let (addr, unfixed, mut value, oops) = yield_complete!(abs_indexed(index, cpu));
        if oops {
            value = cpu.bus.read_u8(addr);
            yield CpuCycle::Tick;
        }
        operation.operate(cpu, value);

        OpTrace::AddrIndexed { addr, unfixed, oops }
    }
}

pub(in crate::cpu) fn x_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    read(operation, cpu.x.get(), cpu)
}

pub(in crate::cpu) fn y_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    read(operation, cpu.y.get(), cpu)
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
fn modify<'a, O: ModifyOperation>(operation: &'a O, index: u8, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let (addr, unfixed, _, _) = yield_complete!(abs_indexed(index, cpu));

        let value = cpu.bus.read_u8(addr);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        yield CpuCycle::Tick;

        // SHX and SHY may override the address to write to.
        let (addr, value) = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);

        OpTrace::AddrIndexed { addr, unfixed, oops: false }    }
}

pub(in crate::cpu) fn x_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    modify(operation, cpu.x.get(), cpu)
}

#[allow(dead_code)]
pub(in crate::cpu) fn y_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    modify(operation, cpu.y.get(), cpu)
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
fn write<'a, O: WriteOperation>(operation: &'a O, index: u8, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let (addr, unfixed, _, _) = yield_complete!(abs_indexed(index, cpu));
        yield CpuCycle::Tick;

        let value = operation.operate(cpu);
        cpu.bus.write_u8(addr, value);

        OpTrace::AddrIndexed { addr, unfixed, oops: false }
    }
}

pub(in crate::cpu) fn x_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    write(operation, cpu.x.get(), cpu)
}

pub(in crate::cpu) fn y_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    write(operation, cpu.y.get(), cpu)
}
