use crate::memory::AddressSpace;
use super::*;

fn ind_x<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = (u16, u8)> + 'a {
    move || {
        let addr = cpu.pc_read_u8_next(mem_map);
        yield CpuCycle::Tick;

        let addr = addr.wrapping_add(cpu.x.get());
        yield CpuCycle::Tick;

        let addr_lo = mem_map.read_u8(addr as u16) as u16;
        yield CpuCycle::Tick;
        let addr_hi = mem_map.read_u8(addr.wrapping_add(1) as u16) as u16;
        yield CpuCycle::Tick;

        let addr = (addr_hi << 8) | addr_lo;
        let value = mem_map.read_u8(addr);
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
pub(super) fn x_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (_, value) = yield_complete!(ind_x(cpu, mem_map));
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
pub(super) fn x_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, value) = yield_complete!(ind_x(cpu, mem_map));
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, value);
        let result = operation.operate(cpu, value);
        yield CpuCycle::Tick;

        mem_map.write_u8(addr, result);
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
pub(super) fn x_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let (addr, _) = yield_complete!(ind_x(cpu, mem_map));
        mem_map.write_u8(addr, operation.operate(cpu));
        yield CpuCycle::Tick;
    }
}

#[allow(dead_code)]
pub(super) fn y_read<'a, O: ReadOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn y_modify<'a, O: ModifyOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn y_write<'a, O: WriteOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}
