use crate::memory::AddressSpace;
use super::*;

fn abs_addr<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = u16> + 'a {
    move || {
        let addr_lo = cpu.pc_read_u8_next(mem_map) as u16;
        yield CpuCycle::Tick;

        let addr_hi = cpu.pc_read_u8_next(mem_map) as u16;
        yield CpuCycle::Tick;

        addr_hi << 8 | addr_lo
    }
}

//  #  address R/W description
// --- ------- --- -------------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low address byte, increment PC
//  3    PC     R  copy low address byte to PCL, fetch high address
//                 byte to PCH
pub(super) fn jmp<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu, mem_map));
        cpu.pc.set(addr);
    }
}

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low byte of address, increment PC
//  3    PC     R  fetch high byte of address, increment PC
//  4  address  R  read from effective address
pub(super) fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu, mem_map));

        let value = mem_map.read_u8(addr);
        operation.operate(cpu, value);
        yield CpuCycle::Tick;
    }
}

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low byte of address, increment PC
//  3    PC     R  fetch high byte of address, increment PC
//  4  address  R  read from effective address
//  5  address  W  write the value back to effective address,
//                 and do the operation on it
//  6  address  W  write the new value to effective address
pub(super) fn modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu, mem_map));

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
//  2    PC     R  fetch low byte of address, increment PC
//  3    PC     R  fetch high byte of address, increment PC
//  4  address  W  write register to effective address
pub(super) fn write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu, mem_map));

        mem_map.write_u8(addr, operation.operate(cpu));
        yield CpuCycle::Tick;
    }
}
