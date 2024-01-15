use super::*;

pub(in crate::cpu) fn interrupt(
    cpu: &Cpu,
    interrupt: Interrupt,
) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    let (interrupt_vector, rst, extra_ticks) = match interrupt {
        Interrupt::Nmi => (0xFFFA, false, 0),
        Interrupt::Brk => (0xFFFE, false, 0),
        //   See start sequence here http://users.telenet.be/kim1-6502/6502/proman.html#92
        Interrupt::Rst => (0xFFFC, true, 2),
    };

    move || {
        if !rst {
            let pc_hi = (cpu.pc.get() >> 8) as u8;
            cpu.push_stack(pc_hi);
            yield CpuCycle::Tick;

            let pc_lo = (cpu.pc.get() & 0x00FF) as u8;
            cpu.push_stack(pc_lo);
            yield CpuCycle::Tick;

            let mut p = cpu.flags.get();
            p.insert(Flags::B | Flags::U);
            cpu.push_stack(p.bits());
            yield CpuCycle::Tick;
        } else {
            // According to http://wiki.nesdev.com/w/index.php/CPU_power_up_state#cite_note-reset-stack-push-3
            // What actually happens is that the pushes still happen, but nothing is written to the stack.
            cpu.sp.set(0xFD);
            cpu.flags.set(Flags::from_bits(0x34).unwrap());
            yield CpuCycle::Tick;
            yield CpuCycle::Tick;
            yield CpuCycle::Tick;
        }
        // http://wiki.nesdev.com/w/index.php/CPU_pin_out_and_signal_description
        // When [the reset button is] released, CPU starts executing code (read $FFFC, read $FFFD, ...) after 6 M2 clocks.
        for _ in 0..extra_ticks {
            yield CpuCycle::Tick;
        }

        let pc_lo = cpu.bus.read_u8(interrupt_vector) as u16;
        yield CpuCycle::Tick;

        cpu.set_flag(Flags::I, true);

        let pc_hi = cpu.bus.read_u8(interrupt_vector.wrapping_add(1)) as u16;
        let pc = pc_hi << 8 | pc_lo;
        cpu.pc.set(pc);

        // Allow the user to override the resulting pc
        if let Some(pc) = cpu.rst_pc {
            cpu.pc.set(pc);
        }

        OpTrace::Implicit
    }
}

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
pub(in crate::cpu) fn brk(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    move || {
        let _ = cpu.next_pc_read_u8();
        yield CpuCycle::Tick;

        yield_complete!(interrupt(cpu, Interrupt::Brk))
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
pub(in crate::cpu) fn jsr(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
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

        OpTrace::Implicit
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
pub(in crate::cpu) fn rti(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
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

        OpTrace::Implicit
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
pub(in crate::cpu) fn rts(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
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

        OpTrace::Implicit
    }
}

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
//  3  $0100,S  W  push register on stack, decrement S
fn push(cpu: &Cpu, value: u8) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    move || {
        let _ = cpu.pc_read_u8();
        yield CpuCycle::Tick;

        cpu.push_stack(value);
        OpTrace::Implicit
    }
}
pub(in crate::cpu) fn php(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    // PHP sets the B flag
    let mut flags = cpu.flags.get();
    flags.set(Flags::B, true);
    push(cpu, flags.bits())
}
pub(in crate::cpu) fn pha(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    push(cpu, cpu.acc.get())
}

//  #  address R/W description
// --- ------- --- -----------------------------------------------
//  1    PC     R  fetch opcode, increment PC
//  2    PC     R  read next instruction byte (and throw it away)
//  3  $0100,S  R  increment S
//  4  $0100,S  R  pull register from stack
fn pull(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = u8> + '_ {
    move || {
        let _ = cpu.pc_read_u8();
        yield CpuCycle::Tick;

        cpu.stack_inc();
        yield CpuCycle::Tick;

        cpu.read_stack()
    }
}

pub(in crate::cpu) fn plp(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    move || {
        let value = yield_complete!(pull(cpu));
        let mut flags = Flags::from_bits_truncate(value);
        flags.remove(Flags::B);
        flags.insert(Flags::U); // this must always be 1
        cpu.flags.set(flags);

        OpTrace::Implicit
    }
}

pub(in crate::cpu) fn pla(cpu: &Cpu) -> impl Coroutine<Yield = CpuCycle, Return = OpTrace> + '_ {
    move || {
        let value = yield_complete!(pull(cpu));
        cpu.acc.set(value);
        cpu.set_flags_from_acc();

        OpTrace::Implicit
    }
}
