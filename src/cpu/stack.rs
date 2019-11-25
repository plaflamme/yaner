use super::*;

//  #  address R/W description
// --- ------- --- -------------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low address byte, increment PC
//  3  $0100,S  R  internal operation (predecrement S?)
//  4  $0100,S  W  push PCH on stack, decrement S
//  5  $0100,S  W  push PCL on stack, decrement S
//  6    PC     R  copy low address byte to PCL, fetch high address
//                 byte to PCH
pub(super) fn jsr<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
    move || {
        let addr_lo = cpu.pc_read_u8_next(mem_map) as u16;
        yield CpuCycle::Tick;

        yield CpuCycle::Tick;

        let pc_hi = (cpu.pc.get() >> 8) as u8;
        cpu.push_stack(mem_map, pc_hi);
        yield CpuCycle::Tick;

        let pc_lo = (cpu.pc.get() & 0x00FF) as u8;
        cpu.push_stack(mem_map, pc_lo);
        yield CpuCycle::Tick;

        let addr_hi = cpu.pc_read_u8_next(mem_map) as u16;
        let pc = addr_hi << 8 | addr_lo;
        cpu.pc.set(pc);
        yield CpuCycle::Tick;
    }
}

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
//  3  $0100,S  R  increment S
//  4  $0100,S  R  pull PCL from stack, increment S
//  5  $0100,S  R  pull PCH from stack
//  6    PC     R  increment PC
pub(super) fn rts<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        let _ = cpu.pc_read_u8_next(mem_map);
        yield CpuCycle::Tick;

        cpu.stack_inc();
        yield CpuCycle::Tick;

        let pc_lo = cpu.pop_stack(mem_map) as u16;
        yield CpuCycle::Tick;

        let pc_hi = cpu.read_stack(mem_map) as u16;
        yield CpuCycle::Tick;

        let pc = ((pc_hi << 8) | pc_lo).wrapping_add(1);
        cpu.pc.set(pc);
        yield CpuCycle::Tick;
    }
}

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
//  3  $0100,S  W  push register on stack, decrement S
fn push<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>, value: u8) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
    move || {
        let _ = cpu.pc_read_u8_next(mem_map);
        yield CpuCycle::Tick;

        cpu.push_stack(mem_map, value);
        yield CpuCycle::Tick;
    }
}
pub(super) fn php<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
    push(cpu, mem_map, cpu.flags.get().bits())
}
pub(super) fn pha<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
    push(cpu, mem_map, cpu.acc.get())
}

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
//  3  $0100,S  R  increment S
//  4  $0100,S  R  pull register from stack
fn pull<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=u8> + 'a {
    move || {
        let _ = cpu.pc_read_u8_next(mem_map);
        yield CpuCycle::Tick;

        cpu.stack_dec();
        yield CpuCycle::Tick;

        cpu.read_stack(mem_map)
    }
}

pub(super) fn plp<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
    move || {
        let value = yield_complete!(pull(cpu, mem_map));
        cpu.flags.set(Flags::from_bits_truncate(value));
        yield CpuCycle::Tick;
    }
}

pub(super) fn pla<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
    move || {
        let value = yield_complete!(pull(cpu, mem_map));
        cpu.acc.set(value);
        yield CpuCycle::Tick;
    }
}
