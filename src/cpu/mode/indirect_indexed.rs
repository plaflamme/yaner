use super::*;
use crate::memory::AddressSpace;

fn ind_x<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = u16> + 'a {
    move || {
        let pointer = cpu.next_pc_read_u8();
        yield CpuCycle::Tick;

        let pointer = pointer.wrapping_add(cpu.x.get());
        yield CpuCycle::Tick;

        let addr_lo = cpu.bus.read_u8(pointer as u16) as u16;
        yield CpuCycle::Tick;
        let addr_hi = cpu.bus.read_u8(pointer.wrapping_add(1) as u16) as u16;
        yield CpuCycle::Tick;

        let addr = (addr_hi << 8) | addr_lo;
        addr
    }
}

//  #    address   R/W description
// --- ----------- --- ------------------------------------------
//  1      PC       R  fetch opcode, increment PC
//  2      PC       R  fetch pointer address, increment PC
//  3    pointer    R  read from the address, add X to it
//  4   pointer+X   R  fetch effective address low
//  5  pointer+X+1  R  fetch effective address high
//  6    address    R  read from effective address
//
// Note: The effective address is always fetched from zero page,
//       i.e. the zero page boundary crossing is not handled.
pub(in crate::cpu) fn x_read<'a, O: ReadOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(ind_x(cpu));
        let value = cpu.bus.read_u8(addr);
        operation.operate(cpu, value);
        OpTrace::Addr(addr)
    }
}

//  #    address   R/W description
// --- ----------- --- ------------------------------------------
//  1      PC       R  fetch opcode, increment PC
//  2      PC       R  fetch pointer address, increment PC
//  3    pointer    R  read from the address, add X to it
//  4   pointer+X   R  fetch effective address low
//  5  pointer+X+1  R  fetch effective address high
//  6    address    R  read from effective address
//  7    address    W  write the value back to effective address,
//                     and do the operation on it
//  8    address    W  write the new value to effective address
//
// Note: The effective address is always fetched from zero page,
//       i.e. the zero page boundary crossing is not handled.
#[allow(dead_code)]
pub(in crate::cpu) fn x_modify<'a, O: ModifyOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(ind_x(cpu));
        let value = cpu.bus.read_u8(addr);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        let (_, result) = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, result);
        OpTrace::Addr(addr)
    }
}

//  #    address   R/W description
// --- ----------- --- ------------------------------------------
//  1      PC       R  fetch opcode, increment PC
//  2      PC       R  fetch pointer address, increment PC
//  3    pointer    R  read from the address, add X to it
//  4   pointer+X   R  fetch effective address low
//  5  pointer+X+1  R  fetch effective address high
//  6    address    W  write to effective address
//
// Note: The effective address is always fetched from zero page,
//       i.e. the zero page boundary crossing is not handled.
pub(in crate::cpu) fn x_write<'a, O: WriteOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(ind_x(cpu));
        cpu.bus.write_u8(addr, operation.operate(cpu));
        OpTrace::Addr(addr)
    }
}

fn ind_y<'a>(
    eager: bool,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = (u16, u16, u8, bool)> + 'a {
    move || {
        let pointer = cpu.next_pc_read_u8();
        yield CpuCycle::Tick;

        let addr_lo = cpu.bus.read_u8(pointer as u16);
        yield CpuCycle::Tick;

        let addr_hi = cpu.bus.read_u8(pointer.wrapping_add(1) as u16) as u16;
        let addr_hi = addr_hi << 8;
        let addr_pre = addr_hi | addr_lo.wrapping_add(cpu.y.get()) as u16;
        yield CpuCycle::Tick;

        let mut value = cpu.bus.read_u8(addr_pre);

        let addr_fixed = (addr_hi | addr_lo as u16).wrapping_add(cpu.y.get() as u16);
        let oops = addr_pre != addr_fixed;
        if eager || oops {
            value = cpu.bus.read_u8(addr_fixed);
            yield CpuCycle::Tick;
        }

        (addr_fixed, addr_pre, value, oops)
    }
}

//  #    address   R/W description
// --- ----------- --- ------------------------------------------
//  1      PC       R  fetch opcode, increment PC
//  2      PC       R  fetch pointer address, increment PC
//  3    pointer    R  fetch effective address low
//  4   pointer+1   R  fetch effective address high,
//                     add Y to low byte of effective address
//  5   address+Y*  R  read from effective address,
//                     fix high byte of effective address
//  6+  address+Y   R  read from effective address
//
// Notes: The effective address is always fetched from zero page,
//        i.e. the zero page boundary crossing is not handled.
//
//        * The high byte of the effective address may be invalid
//          at this time, i.e. it may be smaller by $100.
//
//        + This cycle will be executed only if the effective address
//          was invalid during cycle #5, i.e. page boundary was crossed.
pub(in crate::cpu) fn y_read<'a, O: ReadOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let (addr, unfixed, value, oops) = yield_complete!(ind_y(false, cpu));
        operation.operate(cpu, value);
        OpTrace::AddrIndexed {
            addr,
            unfixed,
            oops,
        }
    }
}

//  #    address   R/W description
// --- ----------- --- ------------------------------------------
//  1      PC       R  fetch opcode, increment PC
//  2      PC       R  fetch pointer address, increment PC
//  3    pointer    R  fetch effective address low
//  4   pointer+1   R  fetch effective address high,
//                     add Y to low byte of effective address
//  5   address+Y*  R  read from effective address,
//                     fix high byte of effective address
//  6   address+Y   R  read from effective address
//  7   address+Y   W  write the value back to effective address,
//                     and do the operation on it
//  8   address+Y   W  write the new value to effective address
//
// Notes: The effective address is always fetched from zero page,
//        i.e. the zero page boundary crossing is not handled.
//
//        * The high byte of the effective address may be invalid
//          at this time, i.e. it may be smaller by $100.
#[allow(dead_code)]
pub(in crate::cpu) fn y_modify<'a, O: ModifyOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let (addr, unfixed, value, _) = yield_complete!(ind_y(true, cpu));
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        let (_, result) = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, result);

        OpTrace::AddrIndexed {
            addr,
            unfixed,
            oops: false,
        }
    }
}

//  #    address   R/W description
// --- ----------- --- ------------------------------------------
//  1      PC       R  fetch opcode, increment PC
//  2      PC       R  fetch pointer address, increment PC
//  3    pointer    R  fetch effective address low
//  4   pointer+1   R  fetch effective address high,
//                     add Y to low byte of effective address
//  5   address+Y*  R  read from effective address,
//                     fix high byte of effective address
//  6   address+Y   W  write to effective address
//
// Notes: The effective address is always fetched from zero page,
//        i.e. the zero page boundary crossing is not handled.
//
//        * The high byte of the effective address may be invalid
//          at this time, i.e. it may be smaller by $100.
pub(in crate::cpu) fn y_write<'a, O: WriteOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let (addr, unfixed, _, _) = yield_complete!(ind_y(true, cpu));
        cpu.bus.write_u8(addr, operation.operate(cpu));

        OpTrace::AddrIndexed {
            addr,
            unfixed,
            oops: false,
        }
    }
}
