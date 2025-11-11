use super::*;
use crate::{memory::AddressSpace, memory_read, memory_write};

fn ind_x(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = u16> + '_ {
    #[coroutine]
    move || {
        let pointer = memory_read! { cpu, cpu.next_pc_read_u8() };

        memory_read! { cpu, cpu.bus.read_u8(pointer as u16) as u16 };

        let pointer = pointer.wrapping_add(cpu.x.get());
        let addr_lo = memory_read! { cpu, cpu.bus.read_u8(pointer as u16) as u16 };
        let addr_hi = memory_read! { cpu, cpu.bus.read_u8(pointer.wrapping_add(1) as u16) as u16 };

        (addr_hi << 8) | addr_lo
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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    #[coroutine]
    move || {
        let addr = yield_complete!(ind_x(cpu));
        let value = memory_read! { cpu, cpu.bus.read_u8(addr) };
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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    #[coroutine]
    move || {
        let addr = yield_complete!(ind_x(cpu));

        let value = memory_read! { cpu, cpu.bus.read_u8(addr) };

        memory_write! { cpu, cpu.bus.write_u8(addr, value) };
        let (_, result) = operation.modify(cpu, addr, value);

        memory_write! { cpu, cpu.bus.write_u8(addr, result) };

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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    #[coroutine]
    move || {
        let addr = yield_complete!(ind_x(cpu));
        memory_write! { cpu, cpu.bus.write_u8(addr, operation.operate(cpu)) };
        OpTrace::Addr(addr)
    }
}

fn ind_y(
    eager: bool,
    cpu: &Cpu,
) -> impl Coroutine<Yield = CpuCycle, Return = (u16, u16, bool)> + '_ {
    #[coroutine]
    move || {
        let pointer = memory_read! { cpu, cpu.next_pc_read_u8() };

        let addr_lo = memory_read! { cpu, cpu.bus.read_u8(pointer as u16) };
        let addr_hi =
            memory_read! { cpu, cpu.bus.read_u8(pointer.wrapping_add(1) as u16) as u16 } << 8;

        let addr_pre = addr_hi | (addr_lo.wrapping_add(cpu.y.get()) as u16);
        let addr_fixed = (addr_hi | (addr_lo as u16)).wrapping_add(cpu.y.get() as u16);

        let oops = addr_pre != addr_fixed;

        if eager || oops {
            memory_read! { cpu, cpu.bus.read_u8(addr_pre) };
        }

        (addr_fixed, addr_pre, oops)
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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    #[coroutine]
    move || {
        let (addr, unfixed, oops) = yield_complete!(ind_y(false, cpu));
        let value = memory_read! { cpu, cpu.bus.read_u8(addr) };
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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    #[coroutine]
    move || {
        let (addr, unfixed, _) = yield_complete!(ind_y(true, cpu));

        let value = memory_read! { cpu, cpu.bus.read_u8(addr) };

        memory_write! { cpu, cpu.bus.write_u8(addr, value) };
        let (_, result) = operation.modify(cpu, addr, value);

        memory_write! { cpu, cpu.bus.write_u8(addr, result) };

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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    #[coroutine]
    move || {
        let (addr, unfixed, _) = yield_complete!(ind_y(true, cpu));

        memory_write! { cpu, cpu.bus.write_u8(addr, operation.operate(cpu)) };

        OpTrace::AddrIndexed {
            addr,
            unfixed,
            oops: false,
        }
    }
}
