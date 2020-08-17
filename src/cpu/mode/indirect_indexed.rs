use crate::memory::AddressSpace;
use super::*;

fn ind_x<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = (u16, u8)> + 'a {
    move || {
        let addr = cpu.pc_read_u8_next();
        yield CpuCycle::Tick;

        let addr = addr.wrapping_add(cpu.x.get());
        yield CpuCycle::Tick;

        let addr_lo = cpu.bus.read_u8(addr as u16) as u16;
        yield CpuCycle::Tick;
        let addr_hi = cpu.bus.read_u8(addr.wrapping_add(1) as u16) as u16;
        yield CpuCycle::Tick;

        let addr = (addr_hi << 8) | addr_lo;
        let value = cpu.bus.read_u8(addr);
        (addr, value)
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
pub(in crate::cpu) fn x_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (_, value) = yield_complete!(ind_x(cpu));
        operation.operate(cpu, value);
        yield CpuCycle::Tick;
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
pub(in crate::cpu) fn x_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, value) = yield_complete!(ind_x(cpu));
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        let (_, result) = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, result);
        yield CpuCycle::Tick;
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
pub(in crate::cpu) fn x_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, _) = yield_complete!(ind_x(cpu));
        cpu.bus.write_u8(addr, operation.operate(cpu));
        yield CpuCycle::Tick;
    }
}

fn ind_y<'a>(eager: bool, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = (u16, u8)> + 'a {
    move || {
        let addr = cpu.pc_read_u8_next();
        yield CpuCycle::Tick;

        let addr_lo = cpu.bus.read_u8(addr as u16);
        yield CpuCycle::Tick;
        let addr_hi = cpu.bus.read_u8(addr.wrapping_add(1) as u16);
        yield CpuCycle::Tick;

        let addr_unfixed = ((addr_hi as u16) << 8) | addr_lo.wrapping_add(cpu.y.get()) as u16;

        let addr_effective = (((addr_hi as u16) << 8) | addr_lo as u16).wrapping_add(cpu.y.get() as u16);
        if eager || addr_unfixed != addr_effective {
            yield CpuCycle::Tick;
        }

        (addr_effective, cpu.bus.read_u8(addr_effective))
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
pub(in crate::cpu) fn y_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (_, value) = yield_complete!(ind_y(false, cpu));
        operation.operate(cpu, value);
        yield CpuCycle::Tick;
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
pub(in crate::cpu) fn y_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, value) = yield_complete!(ind_y(true, cpu));
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        let (_, result) = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, result);
        yield CpuCycle::Tick;
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
pub(in crate::cpu) fn y_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, _) = yield_complete!(ind_y(true, cpu));
        cpu.bus.write_u8(addr, operation.operate(cpu));
        yield CpuCycle::Tick;
    }
}
