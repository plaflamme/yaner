#![feature(coroutines, coroutine_trait)]
use std::{cell::Cell, fmt::Display, ops::Coroutine};

use bitflags::bitflags;

mod operations;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[rustfmt::skip]
pub enum Op {
    // Official operations
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC,
    CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP,
    JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, RTI,
    RTS, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA, 

    // Unofficial operations
    AHX, ALR, ANC, ARR, AXS, DCP, ISC, LAS, LAX, RLA,
    RRA, SAX, SBC, SHX, SHY, SLO, SRE, TAS, XAA,

    // Halting operation
    KIL,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressingMode {
    Imp, // Implied
    Acc, // Accumulator
    Imm, // Immediate
    Zp0, // ZeroPage
    ZpX, // ZeroPageX
    ZpY, // ZeroPageY
    Rel, // Relative
    Abs, // Absolute
    AbX, // AbsoluteX
    AbY, // AbsoluteY
    Ind, // Indirect
    IdX, // IndirectX
    IdY, // IndirectY
}

#[derive(Debug)]
enum OpType {
    Read,
    Modify,
    Write,
    Branch,
    Stack,
    Implicit,
}

impl OpType {
    fn from_op(op: Op) -> Self {
        use Op::*;
        match op {
            ADC => OpType::Read,

            AHX => OpType::Modify,

            ANC => OpType::Read,
            AND => OpType::Read,
            ALR => OpType::Read,
            ARR => OpType::Read,
            ASL => OpType::Modify,
            AXS => OpType::Read,

            BCC => OpType::Branch,
            BCS => OpType::Branch,
            BEQ => OpType::Branch,

            BIT => OpType::Read,

            BMI => OpType::Branch,
            BNE => OpType::Branch,
            BPL => OpType::Branch,

            BRK => OpType::Stack,

            BVC => OpType::Branch,
            BVS => OpType::Branch,

            CLC => OpType::Read,
            CLD => OpType::Implicit,
            CLI => OpType::Read,
            CLV => OpType::Read,

            CMP => OpType::Read,
            CPX => OpType::Read,
            CPY => OpType::Read,

            DCP => OpType::Modify,
            DEC => OpType::Modify,
            DEX => OpType::Read,
            DEY => OpType::Read,
            EOR => OpType::Read,
            INC => OpType::Modify,
            INX => OpType::Read,
            INY => OpType::Read,
            ISC => OpType::Modify,

            JMP => OpType::Implicit,

            JSR => OpType::Stack,
            KIL => OpType::Read,

            LAX => OpType::Read,
            LAS => OpType::Read,

            LDA => OpType::Read,
            LDX => OpType::Read,
            LDY => OpType::Read,

            LSR => OpType::Modify,

            NOP => OpType::Read,

            ORA => OpType::Read,

            PHA => OpType::Stack,
            PHP => OpType::Stack,
            PLA => OpType::Stack,
            PLP => OpType::Stack,

            RLA => OpType::Modify,

            ROL => OpType::Modify,
            ROR => OpType::Modify,

            RRA => OpType::Modify,

            RTI => OpType::Stack,
            RTS => OpType::Stack,

            SAX => OpType::Write,
            SBC => OpType::Read,

            SEC => OpType::Read,
            SED => OpType::Read,
            SEI => OpType::Read,

            SHX => OpType::Modify,
            SHY => OpType::Modify,

            SLO => OpType::Modify,
            SRE => OpType::Modify,

            STA => OpType::Write,
            STX => OpType::Write,
            STY => OpType::Write,

            TAS => OpType::Modify,
            TAX => OpType::Read,
            TAY => OpType::Read,

            TSX => OpType::Read,
            TXA => OpType::Read,
            TXS => OpType::Read,

            TYA => OpType::Read,
            XAA => OpType::Read, // TODO: not implemented
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpCode(pub Op, pub AddressingMode);

impl OpCode {
    pub fn decode(code: u8) -> Self {
        use AddressingMode::*;
        use Op::*;

        #[rustfmt::skip]
        const OPCODES: [OpCode; 256] = [
            // 0x
            OpCode(BRK, Imp), OpCode(ORA, IdX), OpCode(KIL, Imp), OpCode(SLO, IdX),
            OpCode(NOP, Zp0), OpCode(ORA, Zp0), OpCode(ASL, Zp0), OpCode(SLO, Zp0),
            OpCode(PHP, Imp), OpCode(ORA, Imm), OpCode(ASL, Acc), OpCode(ANC, Imm),
            OpCode(NOP, Abs), OpCode(ORA, Abs), OpCode(ASL, Abs), OpCode(SLO, Abs),
            // 1x
            OpCode(BPL, Rel), OpCode(ORA, IdY), OpCode(KIL, Imp), OpCode(SLO, IdY),
            OpCode(NOP, ZpX), OpCode(ORA, ZpX), OpCode(ASL, ZpX), OpCode(SLO, ZpX),
            OpCode(CLC, Imp), OpCode(ORA, AbY), OpCode(NOP, Imp), OpCode(SLO, AbY),
            OpCode(NOP, AbX), OpCode(ORA, AbX), OpCode(ASL, AbX), OpCode(SLO, AbX),
            // 2x
            OpCode(JSR, Abs), OpCode(AND, IdX), OpCode(KIL, Imp), OpCode(RLA, IdX),
            OpCode(BIT, Zp0), OpCode(AND, Zp0), OpCode(ROL, Zp0), OpCode(RLA, Zp0),
            OpCode(PLP, Imp), OpCode(AND, Imm), OpCode(ROL, Acc), OpCode(ANC, Imm),
            OpCode(BIT, Abs), OpCode(AND, Abs), OpCode(ROL, Abs), OpCode(RLA, Abs),
            // 3x
            OpCode(BMI, Rel), OpCode(AND, IdY), OpCode(KIL, Imp), OpCode(RLA, IdY),
            OpCode(NOP, ZpX), OpCode(AND, ZpX), OpCode(ROL, ZpX), OpCode(RLA, ZpX),
            OpCode(SEC, Imp), OpCode(AND, AbY), OpCode(NOP, Imp), OpCode(RLA, AbY),
            OpCode(NOP, AbX), OpCode(AND, AbX), OpCode(ROL, AbX), OpCode(RLA, AbX),
            // 4x
            OpCode(RTI, Imp), OpCode(EOR, IdX), OpCode(KIL, Imp), OpCode(SRE, IdX),
            OpCode(NOP, Zp0), OpCode(EOR, Zp0), OpCode(LSR, Zp0), OpCode(SRE, Zp0),
            OpCode(PHA, Imp), OpCode(EOR, Imm), OpCode(LSR, Acc), OpCode(ALR, Imm),
            OpCode(JMP, Abs), OpCode(EOR, Abs), OpCode(LSR, Abs), OpCode(SRE, Abs),
            // 5x
            OpCode(BVC, Rel), OpCode(EOR, IdY), OpCode(KIL, Imp), OpCode(SRE, IdY),
            OpCode(NOP, ZpX), OpCode(EOR, ZpX), OpCode(LSR, ZpX), OpCode(SRE, ZpX),
            OpCode(CLI, Imp), OpCode(EOR, AbY), OpCode(NOP, Imp), OpCode(SRE, AbY),
            OpCode(NOP, AbX), OpCode(EOR, AbX), OpCode(LSR, AbX), OpCode(SRE, AbX),
            // 6x
            OpCode(RTS, Imp), OpCode(ADC, IdX), OpCode(KIL, Imp), OpCode(RRA, IdX),
            OpCode(NOP, Zp0), OpCode(ADC, Zp0), OpCode(ROR, Zp0), OpCode(RRA, Zp0),
            OpCode(PLA, Imp), OpCode(ADC, Imm), OpCode(ROR, Acc), OpCode(ARR, Imm),
            OpCode(JMP, Ind), OpCode(ADC, Abs), OpCode(ROR, Abs), OpCode(RRA, Abs),
            // 7x
            OpCode(BVS, Rel), OpCode(ADC, IdY), OpCode(KIL, Imp), OpCode(RRA, IdY),
            OpCode(NOP, ZpX), OpCode(ADC, ZpX), OpCode(ROR, ZpX), OpCode(RRA, ZpX),
            OpCode(SEI, Imp), OpCode(ADC, AbY), OpCode(NOP, Imp), OpCode(RRA, AbY),
            OpCode(NOP, AbX), OpCode(ADC, AbX), OpCode(ROR, AbX), OpCode(RRA, AbX),
            // 8x
            OpCode(NOP, Imm), OpCode(STA, IdX), OpCode(NOP, Imm), OpCode(SAX, IdX),
            OpCode(STY, Zp0), OpCode(STA, Zp0), OpCode(STX, Zp0), OpCode(SAX, Zp0),
            OpCode(DEY, Imp), OpCode(NOP, Imm), OpCode(TXA, Imp), OpCode(XAA, Imm),
            OpCode(STY, Abs), OpCode(STA, Abs), OpCode(STX, Abs), OpCode(SAX, Abs),
            // 9x
            OpCode(BCC, Rel), OpCode(STA, IdY), OpCode(KIL, Imp), OpCode(AHX, IdY),
            OpCode(STY, ZpX), OpCode(STA, ZpX), OpCode(STX, ZpY), OpCode(SAX, ZpY),
            OpCode(TYA, Imp), OpCode(STA, AbY), OpCode(TXS, Imp), OpCode(TAS, AbY),
            OpCode(SHY, AbX), OpCode(STA, AbX), OpCode(SHX, AbY), OpCode(AHX, AbY),
            // Ax
            OpCode(LDY, Imm), OpCode(LDA, IdX), OpCode(LDX, Imm), OpCode(LAX, IdX),
            OpCode(LDY, Zp0), OpCode(LDA, Zp0), OpCode(LDX, Zp0), OpCode(LAX, Zp0),
            OpCode(TAY, Imp), OpCode(LDA, Imm), OpCode(TAX, Imp), OpCode(LAX, Imm),
            OpCode(LDY, Abs), OpCode(LDA, Abs), OpCode(LDX, Abs), OpCode(LAX, Abs),
            // Bx
            OpCode(BCS, Rel), OpCode(LDA, IdY), OpCode(KIL, Imp), OpCode(LAX, IdY),
            OpCode(LDY, ZpX), OpCode(LDA, ZpX), OpCode(LDX, ZpY), OpCode(LAX, ZpY),
            OpCode(CLV, Imp), OpCode(LDA, AbY), OpCode(TSX, Imp), OpCode(LAS, AbY),
            OpCode(LDY, AbX), OpCode(LDA, AbX), OpCode(LDX, AbY), OpCode(LAX, AbY),
            // Cx
            OpCode(CPY, Imm), OpCode(CMP, IdX), OpCode(NOP, Imm), OpCode(DCP, IdX),
            OpCode(CPY, Zp0), OpCode(CMP, Zp0), OpCode(DEC, Zp0), OpCode(DCP, Zp0),
            OpCode(INY, Imp), OpCode(CMP, Imm), OpCode(DEX, Imp), OpCode(AXS, Imm),
            OpCode(CPY, Abs), OpCode(CMP, Abs), OpCode(DEC, Abs), OpCode(DCP, Abs),
            // Dx
            OpCode(BNE, Rel), OpCode(CMP, IdY), OpCode(KIL, Imp), OpCode(DCP, IdY),
            OpCode(NOP, ZpX), OpCode(CMP, ZpX), OpCode(DEC, ZpX), OpCode(DCP, ZpX),
            OpCode(CLD, Imp), OpCode(CMP, AbY), OpCode(NOP, Imp), OpCode(DCP, AbY),
            OpCode(NOP, AbX), OpCode(CMP, AbX), OpCode(DEC, AbX), OpCode(DCP, AbX),
            // Ex
            OpCode(CPX, Imm), OpCode(SBC, IdX), OpCode(NOP, Imm), OpCode(ISC, IdX),
            OpCode(CPX, Zp0), OpCode(SBC, Zp0), OpCode(INC, Zp0), OpCode(ISC, Zp0),
            OpCode(INX, Imp), OpCode(SBC, Imm), OpCode(NOP, Imp), OpCode(SBC, Imm),
            OpCode(CPX, Abs), OpCode(SBC, Abs), OpCode(INC, Abs), OpCode(ISC, Abs),
            // Fx
            OpCode(BEQ, Rel), OpCode(SBC, IdY), OpCode(KIL, Imp), OpCode(ISC, IdY),
            OpCode(NOP, ZpX), OpCode(SBC, ZpX), OpCode(INC, ZpX), OpCode(ISC, ZpX),
            OpCode(SED, Imp), OpCode(SBC, AbY), OpCode(NOP, Imp), OpCode(ISC, AbY),
            OpCode(NOP, AbX), OpCode(SBC, AbX), OpCode(INC, AbX), OpCode(ISC, AbX),
        ];

        OPCODES[code as usize]
    }
}

// http://wiki.nesdev.com/w/index.php/Status_flags
bitflags!(
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Flags: u8 {
        // http://wiki.nesdev.com/w/index.php/Status_flags#C:_Carry
        const C = 1 << 0;

        // http://wiki.nesdev.com/w/index.php/Status_flags#Z:_Zero
        const Z = 1 << 1;

        // http://wiki.nesdev.com/w/index.php/Status_flags#I:_Interrupt_Disable
        const I = 1 << 2;

        // http://wiki.nesdev.com/w/index.php/Status_flags#D:_Decimal
        const D = 1 << 3;

        // http://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
        const B = 1 << 4;

        // unused
        const U = 1 << 5;

        // http://wiki.nesdev.com/w/index.php/Status_flags#V:_Overflow
        const V = 1 << 6;

        // http://wiki.nesdev.com/w/index.php/Status_flags#N:_Negative
        const N = 1 << 7;
    }
);

#[derive(Default)]
#[allow(unused)]
struct NmiLine {
    // state of the NMI line input
    line: Cell<bool>,
    // internal state of the NMI line, used to detect false->true edges
    prev_line: Cell<bool>,
}

impl NmiLine {
    fn set(&self) {
        self.line.set(true);
    }

    fn clear(&self) {
        self.line.set(false);
    }
    fn poll(&self) -> bool {
        let polled = !self.prev_line.get() && self.line.get();
        self.prev_line.set(self.line.get());
        polled
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Interrupts: u8 {
        const RST = 1 << 0;
        const IRQ = 1 << 1;
        const NMI = 1 << 2;
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Rw {
    Read,
    Write,
}

#[derive(Debug, Clone, Copy)]
pub enum Phase {
    One, // φ1
    Two, // φ2
}

impl Display for Phase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Phase::One => write!(f, "φ1"),
            Phase::Two => write!(f, "φ2"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CpuEvent {
    Cycle,
    Tick(CpuTick),
}

#[derive(Debug, Clone, Copy)]
pub struct CpuTick {
    pub phase: Phase,
    pub rw: Rw,
    pub addr: u16,
}

pub struct Cpu {
    acc: Cell<u8>,
    x: Cell<u8>,
    y: Cell<u8>,
    flags: Cell<Flags>,
    sp: Cell<u8>,
    pc: Cell<u16>,

    // /NMI line - edge sensitive - high to low
    nmi_line: NmiLine,
    interrupts: Cell<Interrupts>, // Pending interrupts

    rst_pc: Option<u16>, // Optional PC to use after reset instead of reading it from the reset vector
    // Use to delay setting the `I` flag by one instrcution
    delay_intr_flag: Cell<Option<bool>>,
    delay_nmi: Cell<Option<bool>>,
    // The PC of the currently executing instruction
    active_pc: Cell<u16>,

    pub dma_latch: Cell<Option<u8>>,
    pub io_bus: Cell<u8>,
}

impl std::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "a: {:X} x:{:X} y:{:X} sp:{:X} pc:{:X} bus:{:X} p:{:X} ",
            self.acc.get(),
            self.x.get(),
            self.y.get(),
            self.sp.get(),
            self.pc.get(),
            self.io_bus.get(),
            self.flags.get().bits()
        )?;
        bitflags::parser::to_writer(&self.flags.get(), f)
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Cpu::new(None)
    }
}
impl Cpu {
    pub fn new(rst_pc: Option<u16>) -> Self {
        Cpu {
            acc: Cell::new(0),
            x: Cell::new(0),
            y: Cell::new(0),
            flags: Cell::new(Flags::I),
            sp: Cell::new(0x00),
            pc: Cell::new(0),

            nmi_line: NmiLine::default(),
            interrupts: Cell::new(Interrupts::RST),
            rst_pc,

            delay_intr_flag: Cell::default(),
            delay_nmi: Cell::default(),
            active_pc: Cell::default(),

            dma_latch: Cell::default(),
            io_bus: Cell::new(0),
        }
    }

    pub fn set_nmi(&self, state: bool) {
        if state {
            self.nmi_line.set();
        } else {
            self.nmi_line.clear();
        }
    }

    pub fn set_irq(&self, state: bool) {
        if state {
            self.interrupts.update(|i| i | Interrupts::IRQ);
        } else {
            self.interrupts.update(|i| i - Interrupts::IRQ);
        }
    }

    // This should be invoked during the end of each cpu cycle (φ2).
    //
    // During this, we poll the nmi line. If we notice we should trigger it, we set the `delay_nmi` flag.
    //
    // This allows us to also handle φ1, but of the **next** cycle, since we've delayed the signal.
    fn poll_nmi(&self) {
        // "The internal signal goes high during φ1 of the cycle that follows the one where the edge is detected,
        // and stays high until the NMI has been handled."
        if let Some(true) = self.delay_nmi.take() {
            self.interrupts.update(|i| i | Interrupts::NMI);
        }

        // "This edge detector polls the status of the NMI line during φ2 of each CPU cycle (i.e., during the
        // second half of each cycle) and raises an internal signal if the input goes from being high during
        // one cycle to being low during the next"
        if self.nmi_line.poll() {
            self.delay_nmi.set(Some(true));
        }
    }

    pub fn reset(&self) {
        self.interrupts.update(|intr| intr | Interrupts::RST);
    }

    fn flag(&self, f: Flags) -> bool {
        self.flags.get().contains(f)
    }

    fn set_flag(&self, flag: Flags, v: bool) {
        let mut flags = self.flags.get();
        flags.set(flag, v);
        self.flags.set(flags);
    }

    // sets the negative and zero flags
    fn set_flags_from(&self, v: u8) {
        self.set_flag(Flags::N, (v & 0x80) != 0);
        self.set_flag(Flags::Z, v == 0);
    }

    fn set_flags_from_acc(&self) {
        self.set_flags_from(self.acc.get())
    }

    fn start_cycle(&self) {}
    fn end_cycle(&self) {
        self.poll_nmi();
    }

    pub fn run(&self) -> impl Coroutine<Yield = CpuEvent, Return = ()> + '_ {
        macro_rules! cycle {
            ($rw:expr, $addr:expr) => {{
                self.start_cycle();
                yield CpuEvent::Tick(CpuTick {
                    phase: Phase::One,
                    rw: $rw,
                    addr: $addr,
                });
                self.end_cycle();
                yield CpuEvent::Tick(CpuTick {
                    phase: Phase::Two,
                    rw: $rw,
                    addr: $addr,
                });
            }};
        }
        macro_rules! dma {
            ($addr:expr) => {{
                cycle!(Rw::Read, $addr);

                for addr_lo in 0x00..=0xFF {
                    cycle!(Rw::Read, $addr | addr_lo);

                    // 0x2004 is OAMDATA
                    write!(0x2004, self.io_bus.get());
                    if addr_lo == 0xFF {
                        break;
                    }
                }
            }};
        }
        macro_rules! read {
            ( $addr: expr ) => {{
                let addr = $addr;
                if let Some(addr) = self.dma_latch.take() {
                    dma!((addr as u16) << 8);
                }
                cycle!(Rw::Read, addr);
                self.io_bus.get()
            }};
        }
        macro_rules! write {
            ( $addr: expr, $value: expr ) => {{
                let addr = $addr;
                self.io_bus.set($value);
                cycle!(Rw::Write, addr);
            }};
        }
        macro_rules! next_pc {
            () => {{
                let value = read!(self.pc.get());
                self.pc.update(|pc| pc.wrapping_add(1));
                value
            }};
        }

        // https://www.nesdev.org/wiki/Stack
        // "In an empty stack, the stack pointer points to the element where the next value will be stored. It is moved after pushing and before pulling. [...] 6502 and 65816 use an empty stack."
        macro_rules! push {
            ($value:expr) => {
                write!(0x0100 | (self.sp.get() as u16), $value);
                self.sp.update(|sp| sp.wrapping_sub(1));
            };
            () => {
                // During RESET, stack pushes are turned into reads
                read!(0x0100 | (self.sp.get() as u16));
                self.sp.update(|sp| sp.wrapping_sub(1));
            };
        }

        // https://www.nesdev.org/wiki/Stack
        // "In an empty stack, the stack pointer points to the element where the next value will be stored. It is moved after pushing and before pulling. [...] 6502 and 65816 use an empty stack."
        macro_rules! pop {
            () => {{
                self.sp.update(|sp| sp.wrapping_add(1));
                read!(0x0100 | (self.sp.get() as u16))
            }};
        }

        macro_rules! rst {
            () => {{
                // https://www.pagetable.com/?p=410
                read!(0x00FF);
                read!(0x00FF);
                read!(0x00FF);
                self.sp.set(0);
                self.acc.set(0);
                self.x.set(0);
                self.y.set(0);
                self.flags.set(Flags::U | Flags::I);
                push!();
                push!();
                push!(); // At this point the stack pointer is 0xFD
                let pc_lo = read!(0xFFFC) as u16;
                let pc_hi = read!(0xFFFD) as u16;
                self.pc.set(pc_hi << 8 | pc_lo);
                if let Some(rst_pc) = self.rst_pc {
                    self.pc.set(rst_pc);
                }
                self.interrupts.set(Interrupts::empty());
            }};
        }

        macro_rules! interrupt {
            ($flags:expr, $intr:expr) => {{
                push!((self.pc.get() >> 8) as u8);
                push!((self.pc.get() & 0x00FF) as u8);

                // https://www.nesdev.org/wiki/CPU_interrupts#Interrupt_hijacking
                let is_nmi =
                    $intr == Interrupts::NMI || self.interrupts.get().contains(Interrupts::NMI);

                let (jmp_lo, jmp_hi) = if is_nmi {
                    (0xFFFA, 0xFFFB)
                } else {
                    (0xFFFE, 0xFFFF)
                };

                push!(($flags | Flags::U).bits());

                let pcl = read!(jmp_lo);
                self.set_flag(Flags::I, true);
                let pch = read!(jmp_hi);
                self.pc.set((pch as u16) << 8 | pcl as u16);
                self.interrupts.update(|i| {
                    if is_nmi {
                        i - Interrupts::NMI
                    } else {
                        i - $intr
                    }
                });
            }};
        }

        macro_rules! irq {
            () => {
                read!(self.pc.get());
                read!(self.pc.get());
                interrupt!(self.flags.get() - Flags::B, Interrupts::IRQ);
            };
        }

        macro_rules! nmi {
            () => {
                read!(self.pc.get());
                read!(self.pc.get());
                interrupt!(self.flags.get() - Flags::B, Interrupts::NMI);
            };
        }

        macro_rules! process_interrupts {
            () => {
                let pending = self.interrupts.get();
                if pending.contains(Interrupts::RST) {
                    rst!();
                } else if pending.contains(Interrupts::NMI) {
                    nmi!();
                } else if pending.contains(Interrupts::IRQ) && !self.flags.get().contains(Flags::I)
                {
                    irq!();
                }
            };
        }

        //  #  address R/W description
        // --- ------- --- -----------------------------------------------
        //  1    PC     R  fetch opcode, increment PC
        //  2    PC     R  read next instruction byte (and throw it away),
        //                 increment PC
        //  3  $0100,S  W  push PCH on stack (with B flag set), decrement S
        //  4  $0100,S  W  push PCL on stack, decrement S
        //  *** HIJACK: if NMI is asserted up to this point, then brk will jump to the NMI vector instead ***
        //  5  $0100,S  W  push P on stack, decrement S
        //  6   $FFFE   R  fetch PCL
        //  7   $FFFF   R  fetch PCH
        macro_rules! brk {
            () => {{
                let _ = next_pc!();
                interrupt!(self.flags.get() | Flags::B, Interrupts::IRQ);
            }};
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
        macro_rules! jsr {
            () => {{
                let pcl = next_pc!();

                read!(0x0100 | (self.sp.get() as u16));
                push!((self.pc.get() >> 8) as u8);
                push!((self.pc.get() & 0x00FF) as u8);
                let pch = next_pc!();
                self.pc.set((pch as u16) << 8 | pcl as u16)
            }};
        }

        //  #  address R/W description
        // --- ------- --- -----------------------------------------------
        //  1    PC     R  fetch opcode, increment PC
        //  2    PC     R  read next instruction byte (and throw it away)
        //  3  $0100,S  R  increment S
        //  4  $0100,S  R  pull P from stack, increment S
        //  5  $0100,S  R  pull PCL from stack, increment S
        //  6  $0100,S  R  pull PCH from stack
        macro_rules! rti {
            () => {{
                let _ = read!(self.pc.get());
                read!(self.sp.get() as u16);
                let p = pop!();
                let mut flags = Flags::from_bits_truncate(p) | Flags::U;
                flags.remove(Flags::B); // This can't ever be "set" in the actual register
                self.flags.set(flags);
                let pcl = pop!();
                let pch = pop!();
                self.pc.set((pch as u16) << 8 | pcl as u16)
            }};
        }

        //  #  address R/W description
        // --- ------- --- -----------------------------------------------
        //  1    PC     R  fetch opcode, increment PC
        //  2    PC     R  read next instruction byte (and throw it away)
        //  3  $0100,S  R  increment S
        //  4  $0100,S  R  pull PCL from stack, increment S
        //  5  $0100,S  R  pull PCH from stack
        //  6    PC     R  increment PC
        macro_rules! rts {
            () => {{
                let _ = read!(self.pc.get());
                read!(self.sp.get() as u16);
                let pcl = pop!();
                let pch = pop!();
                self.pc.set((pch as u16) << 8 | pcl as u16);
                next_pc!();
            }};
        }

        //  #  address R/W description
        // --- ------- --- -----------------------------------------------
        //  1    PC     R  fetch opcode, increment PC
        //  2    PC     R  read next instruction byte (and throw it away)
        //  3  $0100,S  W  push register on stack, decrement S
        macro_rules! pha {
            () => {{
                let _ = read!(self.pc.get());
                push!(self.acc.get());
            }};
        }

        //  #  address R/W description
        // --- ------- --- -----------------------------------------------
        //  1    PC     R  fetch opcode, increment PC
        //  2    PC     R  read next instruction byte (and throw it away)
        //  3  $0100,S  W  push register on stack, decrement S
        macro_rules! php {
            () => {{
                let _ = read!(self.pc.get());
                push!((self.flags.get() | Flags::B | Flags::U).bits());
            }};
        }

        //  #  address R/W description
        // --- ------- --- -----------------------------------------------
        //  1    PC     R  fetch opcode, increment PC
        //  2    PC     R  read next instruction byte (and throw it away)
        //  3  $0100,S  R  increment S
        //  4  $0100,S  R  pull register from stack
        macro_rules! pla {
            () => {{
                let _ = read!(self.pc.get());
                read!(self.sp.get() as u16);
                self.acc.set(pop!());
                self.set_flags_from_acc();
            }};
        }

        //  #  address R/W description
        // --- ------- --- -----------------------------------------------
        //  1    PC     R  fetch opcode, increment PC
        //  2    PC     R  read next instruction byte (and throw it away)
        //  3  $0100,S  R  increment S
        //  4  $0100,S  R  pull register from stack
        macro_rules! plp {
            () => {{
                let _ = read!(self.pc.get());
                read!(self.sp.get() as u16);
                let flags = Flags::from_bits_truncate(pop!());
                self.set_flag(Flags::U, true);
                self.set_flag(Flags::C, flags.contains(Flags::C));
                self.set_flag(Flags::Z, flags.contains(Flags::Z));
                self.set_flag(Flags::D, flags.contains(Flags::D));
                self.set_flag(Flags::V, flags.contains(Flags::V));
                self.set_flag(Flags::N, flags.contains(Flags::N));

                // Note that the effect of changing I is delayed one instruction because the flag is changed after IRQ is polled,
                // delaying the effect until IRQ is polled in the next instruction like with CLI and SEI.
                self.delay_intr_flag.set(Some(flags.contains(Flags::I)));
            }};
        }

        macro_rules! kil {
            () => {
                return
            };
        }

        #[coroutine]
        move || {
            loop {
                process_interrupts!();

                self.active_pc.set(self.pc.get());
                yield CpuEvent::Cycle;
                let opcode = next_pc!();

                let OpCode(op, mode) = OpCode::decode(opcode);
                let op_type: OpType = OpType::from_op(op);
                let addr = if matches!(op_type, OpType::Stack) {
                    // JSR is Abs, but not really, because the dummy read is on the stack, not the PC
                    // So we just let all Stack operations do their whole memory handling
                    0
                } else {
                    match mode {
                        AddressingMode::Rel => 0,
                        AddressingMode::Imp | AddressingMode::Acc => {
                            read!(self.pc.get());
                            0
                        }
                        AddressingMode::Imm => {
                            let addr = self.pc.get();
                            self.pc.update(|pc| pc.wrapping_add(1));
                            addr
                        }
                        AddressingMode::Zp0 => next_pc!() as u16,
                        AddressingMode::ZpX => {
                            let addr = next_pc!();
                            read!(addr as u16);
                            addr.wrapping_add(self.x.get()) as u16
                        }
                        AddressingMode::ZpY => {
                            let addr = next_pc!();
                            read!(addr as u16);
                            addr.wrapping_add(self.y.get()) as u16
                        }
                        AddressingMode::Abs => {
                            let addr_lo = next_pc!() as u16;
                            let addr_hi = next_pc!() as u16;
                            (addr_hi << 8) | addr_lo
                        }
                        AddressingMode::AbX | AddressingMode::AbY => {
                            let index = if matches!(mode, AddressingMode::AbX) {
                                self.x.get()
                            } else {
                                self.y.get()
                            };
                            let addr_lo = next_pc!();
                            let addr_hi = next_pc!() as u16;
                            let (addr_lo_oops, oops) = addr_lo.overflowing_add(index);
                            let addr = (addr_hi << 8) | addr_lo_oops as u16;

                            if oops && matches!(op_type, OpType::Read)
                                || matches!(op_type, OpType::Modify | OpType::Write)
                            {
                                // Dummy read for modify or write operations or on page cross for reads
                                read!(addr);
                            }

                            ((addr_hi << 8) | addr_lo as u16).wrapping_add(index as u16)
                        }
                        AddressingMode::Ind => {
                            let ptr_lo = next_pc!();
                            let ptr_hi = next_pc!();
                            let ptr = (ptr_hi as u16) << 8 | ptr_lo as u16;
                            let addr_lo = read!(ptr) as u16;
                            let ptr_plus_1 = (ptr & 0xFF00) | ptr_lo.wrapping_add(1) as u16;
                            let addr_hi = read!(ptr_plus_1) as u16;
                            (addr_hi << 8) | addr_lo
                        }
                        AddressingMode::IdX => {
                            let ptr = next_pc!();
                            read!(ptr as u16);
                            let ptr = ptr.wrapping_add(self.x.get());
                            let addr_lo = read!(ptr as u16) as u16;
                            let addr_hi = read!(ptr.wrapping_add(1) as u16) as u16;
                            (addr_hi << 8) | addr_lo
                        }
                        AddressingMode::IdY => {
                            let ptr = next_pc!();
                            let addr_lo = read!(ptr as u16);
                            let addr_hi = read!(ptr.wrapping_add(1) as u16);

                            let (addr_lo, oops) = addr_lo.overflowing_add(self.y.get());
                            let addr = (addr_hi as u16) << 8 | addr_lo as u16;
                            if oops && matches!(op_type, OpType::Read)
                                || matches!(op_type, OpType::Modify | OpType::Write)
                            {
                                // Dummy read for modify or write operations or on page cross for reads
                                read!(addr);
                            }
                            if oops {
                                let addr_hi = addr_hi.wrapping_add(1);
                                (addr_hi as u16) << 8 | addr_lo as u16
                            } else {
                                addr
                            }
                        }
                    }
                };

                macro_rules! fetch {
                    () => {{ read!(addr) }};
                }

                macro_rules! run {
                    (OpType::Read, $op:expr) => {{
                        operations::ReadOperation::operate(&$op, self, fetch!());
                    }};
                    (OpType::Modify, $op:expr) => {{
                        if matches!(mode, AddressingMode::Acc) {
                            let value = self.acc.get();
                            let value = operations::ModifyOperation::operate(&$op, self, value);
                            self.acc.set(value);
                        } else {
                            let value = fetch!();
                            write!(addr, value);
                            let (addr, value) =
                                operations::ModifyOperation::modify(&$op, self, addr, value);
                            write!(addr, value);
                        }
                    }};
                    (OpType::Write, $op:expr) => {{
                        let value = operations::WriteOperation::operate(&$op, self);
                        write!(addr, value);
                    }};
                    (OpType::Implicit, $op:expr) => {{
                        operations::ImplicitOperation::run(&$op, self);
                    }};
                    (OpType::Branch, $op:expr) => {{
                        let operand = next_pc!() as i8;
                        if operations::BranchOperation::branch(&$op, self) {
                            let pc = self.pc.get();
                            let pcl = (pc & 0x00FF) as u8;
                            let pch = ((pc & 0xFF00) >> 8) as u8;
                            read!(pc);

                            let (pcl, carry) = pcl.overflowing_add_signed(operand);
                            let pch = if carry {
                                // page boundary crossing incurs an additional cycle
                                let _ = read!(pc);
                                pch.wrapping_add_signed(operand.signum())
                            } else {
                                pch
                            };

                            let new_pc = (pch as u16) << 8 | pcl as u16;
                            self.pc.set(new_pc);
                        }
                    }};
                }

                match op {
                    Op::ADC => run!(OpType::Read, operations::adc),
                    Op::AND => run!(OpType::Read, operations::and),
                    Op::ASL => run!(OpType::Modify, operations::asl),
                    Op::BCC => run!(OpType::Branch, operations::bcc),
                    Op::BCS => run!(OpType::Branch, operations::bcs),
                    Op::BEQ => run!(OpType::Branch, operations::beq),
                    Op::BIT => run!(OpType::Read, operations::bit),
                    Op::BMI => run!(OpType::Branch, operations::bmi),
                    Op::BNE => run!(OpType::Branch, operations::bne),
                    Op::BPL => run!(OpType::Branch, operations::bpl),
                    Op::BRK => brk!(),
                    Op::BVC => run!(OpType::Branch, operations::bvc),
                    Op::BVS => run!(OpType::Branch, operations::bvs),
                    Op::CLC => run!(OpType::Implicit, operations::clc),
                    Op::CLD => run!(OpType::Implicit, operations::cld),
                    Op::CLI => run!(OpType::Implicit, operations::cli),
                    Op::CLV => run!(OpType::Implicit, operations::clv),
                    Op::CMP => run!(OpType::Read, operations::cmp),
                    Op::CPX => run!(OpType::Read, operations::cpx),
                    Op::CPY => run!(OpType::Read, operations::cpy),
                    Op::DEC => run!(OpType::Modify, operations::dec),
                    Op::DEX => run!(OpType::Implicit, operations::dex),
                    Op::DEY => run!(OpType::Implicit, operations::dey),
                    Op::EOR => run!(OpType::Read, operations::eor),
                    Op::INC => run!(OpType::Modify, operations::inc),
                    Op::INX => run!(OpType::Implicit, operations::inx),
                    Op::INY => run!(OpType::Implicit, operations::iny),
                    Op::JMP => self.pc.set(addr),
                    Op::JSR => jsr!(),
                    Op::LDA => run!(OpType::Read, operations::lda),
                    Op::LDX => run!(OpType::Read, operations::ldx),
                    Op::LDY => run!(OpType::Read, operations::ldy),
                    Op::LSR => run!(OpType::Modify, operations::lsr),
                    Op::NOP => run!(OpType::Implicit, operations::nop),
                    Op::ORA => run!(OpType::Read, operations::ora),
                    Op::PHA => pha!(),
                    Op::PHP => php!(),
                    Op::PLA => pla!(),
                    Op::PLP => plp!(),
                    Op::ROL => run!(OpType::Modify, operations::rol),
                    Op::ROR => run!(OpType::Modify, operations::ror),
                    Op::RTI => rti!(),
                    Op::RTS => rts!(),
                    Op::SEC => run!(OpType::Implicit, operations::sec),
                    Op::SED => run!(OpType::Implicit, operations::sed),
                    Op::SEI => run!(OpType::Implicit, operations::sei),
                    Op::STA => run!(OpType::Write, operations::sta),
                    Op::STX => run!(OpType::Write, operations::stx),
                    Op::STY => run!(OpType::Write, operations::sty),
                    Op::TAX => run!(OpType::Implicit, operations::tax),
                    Op::TAY => run!(OpType::Implicit, operations::tay),
                    Op::TSX => run!(OpType::Implicit, operations::tsx),
                    Op::TXA => run!(OpType::Implicit, operations::txa),
                    Op::TXS => run!(OpType::Implicit, operations::txs),
                    Op::TYA => run!(OpType::Implicit, operations::tya),
                    Op::AHX => run!(OpType::Modify, operations::ahx),
                    Op::ALR => run!(OpType::Read, operations::alr),
                    Op::ANC => run!(OpType::Read, operations::anc),
                    Op::ARR => run!(OpType::Read, operations::arr),
                    Op::AXS => run!(OpType::Read, operations::axs),
                    Op::DCP => run!(OpType::Modify, operations::dcp),
                    Op::ISC => run!(OpType::Modify, operations::isc),
                    Op::LAS => run!(OpType::Read, operations::las),
                    Op::LAX => run!(OpType::Read, operations::lax),
                    Op::RLA => run!(OpType::Modify, operations::rla),
                    Op::RRA => run!(OpType::Modify, operations::rra),
                    Op::SAX => run!(OpType::Write, operations::sax),
                    Op::SBC => run!(OpType::Read, operations::sbc),
                    Op::SHX => run!(OpType::Modify, operations::shx),
                    Op::SHY => run!(OpType::Modify, operations::shy),
                    Op::SLO => run!(OpType::Modify, operations::slo),
                    Op::SRE => run!(OpType::Modify, operations::sre),
                    Op::TAS => run!(OpType::Modify, operations::tas),
                    Op::XAA => run!(OpType::Implicit, operations::xaa),
                    Op::KIL => kil!(),
                }

                // NOTE: I believe this must be done one **whole** instruction later. Not sure.
                if let Some(state) = self.delay_intr_flag.take() {
                    self.set_flag(Flags::I, state);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct CpuState {
    pub active_pc: u16,
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub flags: Flags,
    pub sp: u8,
    pub intr: Option<u16>,
}

impl CpuState {
    pub fn new(cpu: &Cpu) -> Self {
        CpuState {
            active_pc: cpu.active_pc.get(),
            pc: cpu.pc.get(),
            a: cpu.acc.get(),
            x: cpu.x.get(),
            y: cpu.y.get(),
            sp: cpu.sp.get(),
            flags: cpu.flags.get(),
            intr: None,
        }
    }
}

#[cfg(test)]
mod test {

    use core::panic;
    use std::{
        ops::{Coroutine, CoroutineState},
        pin::Pin,
    };

    use crate::{CpuEvent, Phase};

    use super::Cpu;
    use super::CpuTick;
    use super::Rw;

    bitflags::bitflags! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct Pins: u8 {
            const IRQ = 1 << 0;
            const NMI = 1 << 1;
        }
    }

    fn klaus_test(name: &str) {
        // Program's STDIN/OUT addresses, see report.i65
        const STDOUT_ADDR: u16 = 0xF001;
        const STDIN_ADDR: u16 = 0xF004;
        // For the interrupt test, this is the address where the CPU can write to in order to control the NMI and IRQ pins
        const FEEDBACK_ADDR: u16 = 0xBFFC;
        // Program start address
        const PRG_START: u16 = 0x400;

        let mut ram = [0; 0x10000];
        let pgr = std::fs::read(format!(
            "../roms/6502_65C02_functional_tests/bin_files/{name}.bin"
        ))
        .expect("rom is present");

        // Assembler was configured to put the zero_page as 0
        ram[0x0000..pgr.len()].copy_from_slice(&pgr);

        let cpu = Cpu::new(Some(PRG_START));
        let mut cpu_routine = cpu.run();
        let mut stdout = String::new();
        let mut feedback_register = Pins::from_bits_truncate(ram[FEEDBACK_ADDR as usize]);
        loop {
            match Pin::new(&mut cpu_routine).resume(()) {
                CoroutineState::Yielded(CpuEvent::Tick(CpuTick { phase, rw, addr })) => {
                    if matches!(phase, Phase::One) {
                        match rw {
                            Rw::Read => {
                                cpu.io_bus.set(ram[addr as usize]);
                                if addr == STDIN_ADDR {
                                    // program is waiting for input, so it stopped; successfully or not
                                    if stdout.contains("All tests completed, press R to repeat") {
                                        break;
                                    }
                                    panic!(
                                        "Tests failed.\nCpu state: {cpu:?}\nProgram output was:{stdout}"
                                    )
                                }
                            }
                            Rw::Write => {
                                ram[addr as usize] = cpu.io_bus.get();
                                if addr == STDOUT_ADDR {
                                    let c = ram[STDOUT_ADDR as usize] as char;
                                    // uncomment to see output during testing with `cargo test -- --nocapture`
                                    // print!("{c}");
                                    stdout.push(c);
                                }

                                if addr == FEEDBACK_ADDR {
                                    // We should only trigger for the pins that **changed**,
                                    // so we read the value that was written and then remove the current flags from that to found out which ones actually changed
                                    let pins =
                                        Pins::from_bits_truncate(ram[FEEDBACK_ADDR as usize]);
                                    let changed_pins = pins ^ feedback_register;
                                    if changed_pins.contains(Pins::IRQ) {
                                        cpu.set_irq(pins.contains(Pins::IRQ));
                                    }
                                    if changed_pins.contains(Pins::NMI) {
                                        cpu.set_nmi(pins.contains(Pins::NMI));
                                    }
                                    feedback_register = pins;
                                }
                            }
                        }
                    }
                }
                CoroutineState::Yielded(CpuEvent::Cycle) => (),
                CoroutineState::Complete(_) => {
                    panic!("cpu stopped, PC was 0x{:X}", cpu.active_pc.get());
                }
            }
        }
    }

    #[test]
    fn klaus_6502_functional_test() {
        klaus_test("6502_functional_test");
    }

    #[test]
    fn klaus_6502_interrupt_test() {
        klaus_test("6502_interrupt_test");
    }
}
