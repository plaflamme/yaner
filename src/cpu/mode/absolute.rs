use super::*;
use crate::memory::AddressSpace;

fn abs_addr(cpu: &Cpu) -> impl Generator<Yield = CpuCycle, Return = u16> + '_ {
    move || {
        let addr_lo = cpu.next_pc_read_u8() as u16;
        yield CpuCycle::Tick;

        let addr_hi = cpu.next_pc_read_u8() as u16;
        addr_hi << 8 | addr_lo
    }
}

//  #  address R/W description
// --- ------- --- -------------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low address byte, increment PC
//  3    PC     R  copy low address byte to PCL, fetch high address
//                 byte to PCH
pub(in crate::cpu) fn jmp(cpu: &Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + '_ {
    move || {
        let addr = yield_complete!(abs_addr(cpu));
        cpu.pc.set(addr);

        OpTrace::Addr(addr)
    }
}

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low byte of address, increment PC
//  3    PC     R  fetch high byte of address, increment PC
//  4  address  R  read from effective address
pub(in crate::cpu) fn read<'a, O: ReadOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu));
        yield CpuCycle::Tick;

        let value = cpu.bus.read_u8(addr);
        operation.operate(cpu, value);

        OpTrace::Addr(addr)
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
pub(in crate::cpu) fn modify<'a, O: ModifyOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu));
        yield CpuCycle::Tick;

        let value = cpu.bus.read_u8(addr);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, value);
        let (_, result) = operation.modify(cpu, addr, value);
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, result);

        OpTrace::Addr(addr)
    }
}

//  #  address R/W description
// --- ------- --- ------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low byte of address, increment PC
//  3    PC     R  fetch high byte of address, increment PC
//  4  address  W  write register to effective address
pub(in crate::cpu) fn write<'a, O: WriteOperation>(
    operation: &'a O,
    cpu: &'a Cpu,
) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu));
        yield CpuCycle::Tick;

        cpu.bus.write_u8(addr, operation.operate(cpu));
        OpTrace::Addr(addr)
    }
}
