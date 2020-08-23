use super::*;

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away),
//                 increment PC
//  3  $0100,S  W  push PCH on stack (with B flag set), decrement S
//  4  $0100,S  W  push PCL on stack, decrement S
//  5  $0100,S  W  push P on stack, decrement S
//  6   $FFFE   R  fetch PCL
//  7   $FFFF   R  fetch PCH
pub(in crate::cpu) fn brk<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let _ = cpu.next_pc_read_u8();
        yield CpuCycle::Tick;

        let pc_hi = (cpu.pc.get() >> 8) as u8;
        cpu.push_stack(pc_hi);
        yield CpuCycle::Tick;

        let pc_lo = (cpu.pc.get() & 0x00FF) as u8;
        cpu.push_stack( pc_lo);
        yield CpuCycle::Tick;

        let mut p = cpu.flags.get();
        p.insert(Flags::B | Flags::U);
        cpu.push_stack(p.bits());
        yield CpuCycle::Tick;

        let pc_lo = cpu.bus.read_u8(0xFFFE) as u16;
        yield CpuCycle::Tick;

        cpu.set_flag(Flags::I, true);

        let pc_hi = cpu.bus.read_u8(0xFFFF) as u16;
        let pc = pc_hi << 8 | pc_lo;
        cpu.pc.set(pc);

        OpTrace{}
    }
}

//  #  address R/W description
// --- ------- --- -------------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  fetch low address byte, increment PC
//  3  $0100,S  R  internal operation (predecrement S?)
//  4  $0100,S  W  push PCH on stack, decrement S
//  5  $0100,S  W  push PCL on stack, decrement S
//  6    PC     R  copy low address byte to PCL, fetch high address
//                 byte to PCH
pub(in crate::cpu) fn jsr<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let addr_lo = cpu.next_pc_read_u8() as u16;
        yield CpuCycle::Tick;

        yield CpuCycle::Tick;

        let pc_hi = (cpu.pc.get() >> 8) as u8;
        cpu.push_stack(pc_hi);
        yield CpuCycle::Tick;

        let pc_lo = (cpu.pc.get() & 0x00FF) as u8;
        cpu.push_stack(pc_lo);
        yield CpuCycle::Tick;

        let addr_hi = cpu.next_pc_read_u8() as u16;
        let pc = addr_hi << 8 | addr_lo;
        cpu.pc.set(pc);

        OpTrace{}
    }
}

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
//  3  $0100,S  R  increment S
//  4  $0100,S  R  pull P from stack, increment S
//  5  $0100,S  R  pull PCL from stack, increment S
//  6  $0100,S  R  pull PCH from stack
pub(in crate::cpu) fn rti<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let _ = cpu.next_pc_read_u8();
        yield CpuCycle::Tick;

        cpu.stack_inc();
        yield CpuCycle::Tick;

        let status = cpu.pop_stack();
        let mut flags = Flags::from_bits_truncate(status);
        flags.insert(Flags::U); // this should always be set
        cpu.flags.set(flags);
        yield CpuCycle::Tick;

        let pc_lo = cpu.pop_stack() as u16;
        yield CpuCycle::Tick;

        let pc_hi = cpu.read_stack() as u16;
        cpu.pc.set((pc_hi << 8) | pc_lo);

        OpTrace{}
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
pub(in crate::cpu) fn rts<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let _ = cpu.next_pc_read_u8();
        yield CpuCycle::Tick;

        cpu.stack_inc();
        yield CpuCycle::Tick;

        let pc_lo = cpu.pop_stack() as u16;
        yield CpuCycle::Tick;

        let pc_hi = cpu.read_stack() as u16;
        yield CpuCycle::Tick;

        let pc = ((pc_hi << 8) | pc_lo).wrapping_add(1);
        cpu.pc.set(pc);

        OpTrace{}
    }
}

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
//  3  $0100,S  W  push register on stack, decrement S
fn push<'a>(cpu: &'a Cpu, value: u8) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let _ = cpu.pc_read_u8();
        yield CpuCycle::Tick;

        cpu.push_stack(value);
        OpTrace{}
    }
}
pub(in crate::cpu) fn php<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    // PHP sets the B flag
    let mut flags = cpu.flags.get();
    flags.set(Flags::B, true);
    push(cpu, flags.bits())
}
pub(in crate::cpu) fn pha<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    push(cpu, cpu.acc.get())
}

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
//  3  $0100,S  R  increment S
//  4  $0100,S  R  pull register from stack
fn pull<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = u8> + 'a {
    move || {
        let _ = cpu.pc_read_u8();
        yield CpuCycle::Tick;

        cpu.stack_inc();
        yield CpuCycle::Tick;

        cpu.read_stack()
    }
}

pub(in crate::cpu) fn plp<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let value = yield_complete!(pull(cpu));
        let mut flags = Flags::from_bits_truncate(value);
        flags.remove(Flags::B);
        flags.insert(Flags::U); // this must always be 1
        cpu.flags.set(flags);

        OpTrace{}
    }
}

pub(in crate::cpu) fn pla<'a>(cpu: &'a Cpu) -> impl Generator<Yield = CpuCycle, Return = OpTrace> + 'a {
    move || {
        let value = yield_complete!(pull(cpu));
        cpu.acc.set(value);
        cpu.set_flags_from_acc();

        OpTrace{}
    }
}
