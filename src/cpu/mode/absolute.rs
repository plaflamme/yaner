use super::*;
use crate::{memory::AddressSpace, memory_read, memory_write};

fn abs_addr(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = u16> + '_ {
    move || {
        let addr_lo = memory_read! { cpu, cpu.next_pc_read_u8() as u16 };
        let addr_hi = memory_read! { cpu, cpu.next_pc_read_u8() as u16 };
        addr_hi << 8 | addr_lo
    }
}

//  #  address R/W description
// --- ------- --- -------------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low address byte, increment PC
//  3    PC     R  copy low address byte to PCL, fetch high address
//                 byte to PCH
pub(in crate::cpu) fn jmp(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    move || {
        let addr_lo = memory_read! { cpu, cpu.next_pc_read_u8() as u16 };

        let addr = memory_read! { cpu, (cpu.pc_read_u8() as u16) << 8 | addr_lo };
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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu));

        let value = memory_read! { cpu, cpu.bus.read_u8(addr) };
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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu));

        let value = memory_read! { cpu, cpu.bus.read_u8(addr) };

        memory_write! { cpu, cpu.bus.write_u8(addr, value) };

        let (_, result) = operation.modify(cpu, addr, value);

        memory_write! { cpu, cpu.bus.write_u8(addr, result) };

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
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr = yield_complete!(abs_addr(cpu));

        memory_write! { cpu, cpu.bus.write_u8(addr, operation.operate(cpu)) };
        OpTrace::Addr(addr)
    }
}
