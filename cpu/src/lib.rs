#![feature(coroutines, coroutine_trait)]
use std::{cell::Cell, ops::Coroutine};

use bitflags::bitflags;

mod operations;

use operations::{BranchOperation, ModifyOperation, ReadOperation, WriteOperation};

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

#[allow(unused)]
enum OpType {
    Read,
    Modify,
    Write,
    Branch,
    Stack,
    Implicit,
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
            OpCode(PHP, Imp), OpCode(ORA, Imm), OpCode(ASL, Imp), OpCode(ANC, Imm),
            OpCode(NOP, Abs), OpCode(ORA, Abs), OpCode(ASL, Abs), OpCode(SLO, Abs),
            // 1x
            OpCode(BPL, Rel), OpCode(ORA, IdY), OpCode(KIL, Imp), OpCode(SLO, IdY),
            OpCode(NOP, ZpX), OpCode(ORA, ZpX), OpCode(ASL, ZpX), OpCode(SLO, ZpX),
            OpCode(CLC, Imp), OpCode(ORA, AbY), OpCode(NOP, Imp), OpCode(SLO, AbY),
            OpCode(NOP, AbX), OpCode(ORA, AbX), OpCode(ASL, AbX), OpCode(SLO, AbX),
            // 2x
            OpCode(JSR, Abs), OpCode(AND, IdX), OpCode(KIL, Imp), OpCode(RLA, IdX),
            OpCode(BIT, Zp0), OpCode(AND, Zp0), OpCode(ROL, Zp0), OpCode(RLA, Zp0),
            OpCode(PLP, Imp), OpCode(AND, Imm), OpCode(ROL, Imp), OpCode(ANC, Imm),
            OpCode(BIT, Abs), OpCode(AND, Abs), OpCode(ROL, Abs), OpCode(RLA, Abs),
            // 3x
            OpCode(BMI, Rel), OpCode(AND, IdY), OpCode(KIL, Imp), OpCode(RLA, IdY),
            OpCode(NOP, ZpX), OpCode(AND, ZpX), OpCode(ROL, ZpX), OpCode(RLA, ZpX),
            OpCode(SEC, Imp), OpCode(AND, AbY), OpCode(NOP, Imp), OpCode(RLA, AbY),
            OpCode(NOP, AbX), OpCode(AND, AbX), OpCode(ROL, AbX), OpCode(RLA, AbX),
            // 4x
            OpCode(RTI, Imp), OpCode(EOR, IdX), OpCode(KIL, Imp), OpCode(SRE, IdX),
            OpCode(NOP, Zp0), OpCode(EOR, Zp0), OpCode(LSR, Zp0), OpCode(SRE, Zp0),
            OpCode(PHA, Imp), OpCode(EOR, Imm), OpCode(LSR, Imp), OpCode(ALR, Imm),
            OpCode(JMP, Abs), OpCode(EOR, Abs), OpCode(LSR, Abs), OpCode(SRE, Abs),
            // 5x
            OpCode(BVC, Rel), OpCode(EOR, IdY), OpCode(KIL, Imp), OpCode(SRE, IdY),
            OpCode(NOP, ZpX), OpCode(EOR, ZpX), OpCode(LSR, ZpX), OpCode(SRE, ZpX),
            OpCode(CLI, Imp), OpCode(EOR, AbY), OpCode(NOP, Imp), OpCode(SRE, AbY),
            OpCode(NOP, AbX), OpCode(EOR, AbX), OpCode(LSR, AbX), OpCode(SRE, AbX),
            // 6x
            OpCode(RTS, Imp), OpCode(ADC, IdX), OpCode(KIL, Imp), OpCode(RRA, IdX),
            OpCode(NOP, Zp0), OpCode(ADC, Zp0), OpCode(ROR, Zp0), OpCode(RRA, Zp0),
            OpCode(PLA, Imp), OpCode(ADC, Imm), OpCode(ROR, Imp), OpCode(ARR, Imm),
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

    // whether NMI should occur, this is only reset within the handler
    need_nmi: Cell<bool>,
    // when NMI is determined to need to occur, it should only happen for the next instruction
    // so this flag is set before need_nmi changes
    prev_need_nmi: Cell<bool>,
}

impl NmiLine {
    fn set(&self) {
        self.line.set(true);
    }

    fn clear(&self) {
        self.line.set(false);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Rw {
    Read,
    Write,
}

#[derive(Debug, Clone, Copy)]
pub struct CpuTick {
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

    #[allow(unused)]
    // /NMI line - edge sensitive - high to low
    nmi_line: NmiLine,
    #[allow(unused)]
    // IRQ line - level sensitive
    irq_line: Cell<bool>,
    #[allow(unused)]
    // /RESET line - held low to reset CPU
    pub rst_line: Cell<bool>,
    rst_pc: Option<u16>, // Optional PC to use after reset instead of reading it from the reset vector

    // Use to delay setting the `I` flag by one instrcution
    delay_intr_flag: Cell<Option<bool>>,
    // The PC of the currently executing instruction
    active_pc: Cell<u16>,

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
            irq_line: Cell::new(false),
            rst_line: Cell::new(true),
            rst_pc,

            delay_intr_flag: Cell::default(),
            active_pc: Cell::default(),

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

    pub fn run(&self) -> impl Coroutine<Yield = CpuTick, Return = ()> + '_ {
        macro_rules! start_cycle {
            () => {};
        }
        macro_rules! end_cycle {
            () => {{
                // pretty much verbatim from Mesen... I should figure out how this actually works and clean this up

                // "The internal signal goes high during φ1 of the cycle that follows the one where the edge is detected,
                // and stays high until the NMI has been handled. "
                self.nmi_line.prev_need_nmi.set(self.nmi_line.need_nmi.get());

                // "This edge detector polls the status of the NMI line during φ2 of each CPU cycle (i.e., during the
                // second half of each cycle) and raises an internal signal if the input goes from being high during
                // one cycle to being low during the next"
                if !self.nmi_line.prev_line.get() && self.nmi_line.line.get() {
                    self.nmi_line.need_nmi.set(true);
                }
                self.nmi_line.prev_line.set(self.nmi_line.line.get());
            }};
        }
        macro_rules! read {
            ( $addr: expr ) => {{
                start_cycle!();
                yield CpuTick {
                    rw: Rw::Read,
                    addr: $addr,
                };
                let value = self.io_bus.get();
                end_cycle!();
                value
            }};
        }
        macro_rules! write {
            ( $addr: expr, $value: expr ) => {{
                let addr = $addr;
                start_cycle!();
                self.io_bus.set($value);
                yield CpuTick {
                    rw: Rw::Write,
                    addr,
                };
                end_cycle!();
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
                self.rst_line.set(false);
            }};
        }

        macro_rules! interrupt {
            ($flags:expr, nmi: $is_nmi:literal) => {{
                push!((self.pc.get() >> 8) as u8);
                push!((self.pc.get() & 0x00FF) as u8);

                // https://www.nesdev.org/wiki/CPU_interrupts#Interrupt_hijacking
                let (jmp_lo, jmp_hi, hijacked) = if $is_nmi || self.nmi_line.need_nmi.get() {
                    (0xFFFA, 0xFFFB, !$is_nmi)
                } else {
                    (0xFFFE, 0xFFFF, false)
                };

                push!(($flags | Flags::U).bits());

                let pcl = read!(jmp_lo);
                self.set_flag(Flags::I, true);
                let pch = read!(jmp_hi);
                self.pc.set((pch as u16) << 8 | pcl as u16);

                if $is_nmi || hijacked {
                    self.nmi_line.need_nmi.set(false);
                } else {
                    self.irq_line.set(false);
                }
            }};
        }

        macro_rules! irq {
            () => {
                read!(self.pc.get());
                read!(self.pc.get());
                interrupt!(self.flags.get() - Flags::B, nmi: false);
            };
        }

        macro_rules! nmi {
            () => {
                read!(self.pc.get());
                read!(self.pc.get());
                interrupt!(self.flags.get() - Flags::B, nmi: true);
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
                interrupt!(self.flags.get() | Flags::B, nmi: false);
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

        macro_rules! abs {
            () => {{
                let addr_lo = next_pc!() as u16;
                let addr_hi = next_pc!() as u16;
                (addr_hi << 8) | addr_lo
            }};

            //  #  address R/W description
            // --- ------- --- -------------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  fetch low address byte, increment PC
            //  3    PC     R  copy low address byte to PCL, fetch high address
            //                 byte to PCH
            (OpType::Implicit, operations::jmp) => {{
                self.pc.set(abs!());
            }};

            (OpType::Stack, $op:expr) => {{ $op }};

            //  #  address R/W description
            // --- ------- --- ------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  fetch low byte of address, increment PC
            //  3    PC     R  fetch high byte of address, increment PC
            //  4  address  R  read from effective address
            (OpType::Read, $op: expr) => {{
                $op.operate(self, read!(abs!()));
            }};

            //  #  address R/W description
            // --- ------- --- ------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  fetch low byte of address, increment PC
            //  3    PC     R  fetch high byte of address, increment PC
            //  4  address  R  read from effective address
            //  5  address  W  write the value back to effective address,
            //                 and do the operation on it
            //  6  address  W  write the new value to effective address
            (OpType::Modify, $op: expr) => {{
                let addr = abs!();
                let value = read!(addr);
                write!(addr, value);
                let (addr, value) = $op.modify(self, addr, value);
                write!(addr, value);
            }};

            //  #  address R/W description
            // --- ------- --- ------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  fetch low byte of address, increment PC
            //  3    PC     R  fetch high byte of address, increment PC
            //  4  address  W  write register to effective address
            (OpType::Write, $op: expr) => {{
                write!(abs!(), $op.operate(self));
            }};
        }

        macro_rules! abs_indexed {
            ($index: expr) => {{
                let addr_lo = next_pc!();
                let addr_hi = next_pc!() as u16;
                let (addr_lo_oops, oops) = addr_lo.overflowing_add($index);
                let addr = (addr_hi << 8) | addr_lo_oops as u16;
                let value = read!(addr);
                let addr = ((addr_hi << 8) | addr_lo as u16).wrapping_add($index as u16);
                (addr, value, oops)
            }};

            //  #   address  R/W description
            // --- --------- --- ------------------------------------------
            //  1     PC      R  fetch opcode, increment PC
            //  2     PC      R  fetch low byte of address, increment PC
            //  3     PC      R  fetch high byte of address,
            //                   add index register to low address byte,
            //                   increment PC
            //  4  address+I* R  read from effective address,
            //                   fix the high byte of effective address
            //  5+ address+I  R  re-read from effective address
            //
            //  Notes: I denotes either index register (X or Y).
            //
            //        * The high byte of the effective address may be invalid
            //          at this time, i.e. it may be smaller by $100.
            //
            //        + This cycle will be executed only if the effective address
            //          was invalid during cycle #4, i.e. page boundary was crossed.
            (OpType::Read, $op: expr, $index: expr) => {{
                let (addr, value, oops) = abs_indexed!($index);
                if oops {
                    $op.operate(self, read!(addr));
                } else {
                    $op.operate(self, value);
                }
            }};
            //  #   address  R/W description
            // --- --------- --- ------------------------------------------
            //  1    PC       R  fetch opcode, increment PC
            //  2    PC       R  fetch low byte of address, increment PC
            //  3    PC       R  fetch high byte of address,
            //                   add index register X to low address byte,
            //                   increment PC
            //  4  address+X* R  read from effective address,
            //                   fix the high byte of effective address
            //  5  address+X  R  re-read from effective address
            //  6  address+X  W  write the value back to effective address,
            //                   and do the operation on it
            //  7  address+X  W  write the new value to effective address
            //
            // Notes: * The high byte of the effective address may be invalid
            //          at this time, i.e. it may be smaller by $100.
            (OpType::Modify, $op: expr, $index: expr) => {
                let (addr, _, _) = abs_indexed!($index);
                let value = read!(addr);
                write!(addr, value);
                let (addr, value) = $op.modify(self, addr, value);
                write!(addr, value);
            };
            //  #   address  R/W description
            // --- --------- --- ------------------------------------------
            //  1     PC      R  fetch opcode, increment PC
            //  2     PC      R  fetch low byte of address, increment PC
            //  3     PC      R  fetch high byte of address,
            //                   add index register to low address byte,
            //                   increment PC
            //  4  address+I* R  read from effective address,
            //                   fix the high byte of effective address
            //  5  address+I  W  write to effective address
            //
            // Notes: I denotes either index register (X or Y).
            //
            //        * The high byte of the effective address may be invalid
            //          at this time, i.e. it may be smaller by $100. Because
            //          the processor cannot undo a write to an invalid
            //          address, it always reads from the address first.
            (OpType::Write, $op: expr, $index: expr) => {
                let (addr, _, _) = abs_indexed!($index);
                write!(addr, $op.operate(self));
            };
        }

        //  #  address R/W description
        // --- ------- --- -----------------------------------------------
        //  1    PC     R  fetch opcode, increment PC
        //  2    PC     R  read next instruction byte (and throw it away)
        macro_rules! acc {
            ($op: expr) => {{
                read!(self.pc.get());
                let value = self.acc.get();
                let (_, value) = $op.modify(self, 0, value);
                self.acc.set(value);
            }};
        }

        macro_rules! imp {
            //  #  address R/W description
            // --- ------- --- -----------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  read next instruction byte (and throw it away)
            ($op: expr) => {{
                let value = read!(self.pc.get());
                $op.operate(self, value);
            }};
        }

        macro_rules! zp {
            //  #  address R/W description
            // --- ------- --- ------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  fetch address, increment PC
            //  3  address  R  read from effective address
            (OpType::Read, $op: expr) => {{
                let addr = next_pc!() as u16;
                let value = read!(addr);
                $op.operate(self, value);
            }};
            //  #  address R/W description
            // --- ------- --- ------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  fetch address, increment PC
            //  3  address  R  read from effective address
            //  4  address  W  write the value back to effective address,
            //                 and do the operation on it
            //  5  address  W  write the new value to effective address
            (OpType::Modify, $op: expr) => {{
                let addr = next_pc!() as u16;
                let value = read!(addr);
                write!(addr, value);
                let (addr, value) = $op.modify(self, addr, value);
                write!(addr, value);
            }};

            //  #  address R/W description
            // --- ------- --- ------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  fetch address, increment PC
            //  3  address  W  write register to effective address
            (OpType::Write, $op: expr) => {{
                let addr = next_pc!() as u16;
                write!(addr, $op.operate(self));
            }};
        }

        macro_rules! imm {
            //  #  address R/W description
            // --- ------- --- ------------------------------------------
            //  1    PC     R  fetch opcode, increment PC
            //  2    PC     R  fetch value, increment PC
            (OpType::Read, $op: expr) => {{
                let value = next_pc!();
                $op.operate(self, value);
            }};
        }

        macro_rules! ind {
            //  #   address  R/W description
            // --- --------- --- ------------------------------------------
            //  1     PC      R  fetch opcode, increment PC
            //  2     PC      R  fetch pointer address low, increment PC
            //  3     PC      R  fetch pointer address high, increment PC
            //  4   pointer   R  fetch low address to latch
            //  5  pointer+1* R  fetch PCH, copy latch to PCL
            //
            // Note: * The PCH will always be fetched from the same page
            //         than PCL, i.e. page boundary crossing is not handled.
            (OpType::Implicit, operations::jmp) => {{
                let ptr_lo = next_pc!();
                let ptr_hi = next_pc!();
                let ptr = (ptr_hi as u16) << 8 | ptr_lo as u16;
                let addr_lo = read!(ptr) as u16;
                let ptr_plus_1 = (ptr & 0xFF00) | ptr_lo.wrapping_add(1) as u16;
                let addr_hi = read!(ptr_plus_1) as u16;
                let addr = (addr_hi << 8) | addr_lo;
                self.pc.set(addr);
            }};
        }

        macro_rules! ind_indexed_x {
            () => {{
                let ptr = next_pc!();
                read!(ptr as u16);
                let ptr = ptr.wrapping_add(self.x.get());
                let addr_lo = read!(ptr as u16) as u16;
                let addr_hi = read!(ptr.wrapping_add(1) as u16) as u16;
                (addr_hi << 8) | addr_lo
            }};

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
            (OpType::Read, $op: expr) => {{
                let addr = ind_indexed_x!();
                $op.operate(self, read!(addr));
            }};

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
            (OpType::Modify, $op: expr) => {{
                let addr = ind_indexed_x!();
                let value = read!(addr);
                write!(addr, value);
                let (addr, value) = $op.modify(self, addr, value);
                write!(addr, value);
            }};
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
            (OpType::Write, $op: expr) => {{
                let addr = ind_indexed_x!();
                write!(addr, $op.operate(self));
            }};
        }

        macro_rules! ind_indexed_y {
            () => {{
                let ptr = next_pc!();
                let addr_lo = read!(ptr as u16);
                let addr_hi = read!(ptr.wrapping_add(1) as u16);

                let (addr_lo, carry) = addr_lo.overflowing_add(self.y.get());
                let addr = (addr_hi as u16) << 8 | addr_lo as u16;
                let value = read!(addr);

                if carry {
                    let addr_hi = addr_hi.wrapping_add(1);
                    let addr = (addr_hi as u16) << 8 | addr_lo as u16;
                    let value = read!((addr_hi as u16) << 8 | addr_lo as u16);
                    (addr, value)
                } else {
                    (addr, value)
                }
            }};

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
            (OpType::Read, $op: expr) => {{
                let (_, value) = ind_indexed_y!();
                $op.operate(self, value);
            }};

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
            (OpType::Modify, $op: expr) => {{
                let (addr, value) = ind_indexed_y!();
                write!(addr, value);
                let (addr, value) = $op.modify(self, addr, value);
                write!(addr, value);
            }};

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
            (OpType::Write, $op: expr) => {{
                let (addr, _) = ind_indexed_y!();
                write!(addr, $op.operate(self));
            }};
        }

        macro_rules! rel {
            //  #   address  R/W description
            // --- --------- --- ---------------------------------------------
            //  1     PC      R  fetch opcode, increment PC
            //  2     PC      R  fetch operand, increment PC
            //  3     PC      R  Fetch opcode of next instruction,
            //                   If branch is taken, add operand to PCL.
            //                   Otherwise increment PC.
            //  4+    PC*     R  Fetch opcode of next instruction.
            //                   Fix PCH. If it did not change, increment PC.
            //  5!    PC      R  Fetch opcode of next instruction,
            //                   increment PC.
            //
            // Notes: The opcode fetch of the next instruction is included to
            //        this diagram for illustration purposes. When determining
            //        real execution times, remember to subtract the last
            //        cycle.
            //
            //        * The high byte of Program Counter (PCH) may be invalid
            //          at this time, i.e. it may be smaller or bigger by $100.
            //
            //        + If branch is taken, this cycle will be executed.
            //
            //        ! If branch occurs to different page, this cycle will be
            //          executed.
            (OpType::Branch, $op: expr) => {{
                let operand = next_pc!() as i8;
                if $op.branch(self) {
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

        macro_rules! zp_indexed {
            ($index: expr) => {{
                let addr = next_pc!();
                read!(addr as u16);
                addr.wrapping_add($index) as u16
            }};
            //  #   address  R/W description
            // --- --------- --- ------------------------------------------
            //  1     PC      R  fetch opcode, increment PC
            //  2     PC      R  fetch address, increment PC
            //  3   address   R  read from address, add index register to it
            //  4  address+I* R  read from effective address
            //  Notes: I denotes either index register (X or Y).
            //
            //        * The high byte of the effective address is always zero,
            //          i.e. page boundary crossings are not handled.
            (OpType::Read, $op: expr, $index: expr) => {{
                let addr = zp_indexed!($index);
                let value = read!(addr);
                $op.operate(self, value);
            }};
            //  #   address  R/W description
            // --- --------- --- ---------------------------------------------
            //  1     PC      R  fetch opcode, increment PC
            //  2     PC      R  fetch address, increment PC
            //  3   address   R  read from address, add index register X to it
            //  4  address+X* R  read from effective address
            //  5  address+X* W  write the value back to effective address,
            //                   and do the operation on it
            //  6  address+X* W  write the new value to effective address
            //
            // Note: * The high byte of the effective address is always zero,
            //         i.e. page boundary crossings are not handled.
            (OpType::Modify, $op: expr, $index: expr) => {
                let addr = zp_indexed!($index);
                let value = read!(addr);
                write!(addr, value);
                let (addr, value) = $op.modify(self, addr, value);
                write!(addr, value);
            };
            //  #   address  R/W description
            // --- --------- --- -------------------------------------------
            //  1     PC      R  fetch opcode, increment PC
            //  2     PC      R  fetch address, increment PC
            //  3   address   R  read from address, add index register to it
            //  4  address+I* W  write to effective address
            //
            // Notes: I denotes either index register (X or Y).
            //
            //        * The high byte of the effective address is always zero,
            //          i.e. page boundary crossings are not handled.
            (OpType::Write, $op: expr, $index: expr) => {
                let addr = zp_indexed!($index);
                write!(addr, $op.operate(self));
            };
        }

        macro_rules! kil {
            () => {
                return
            };
        }

        #[coroutine]
        move || {
            loop {
                if self.rst_line.get() {
                    rst!();
                } else if self.nmi_line.prev_need_nmi.get() {
                    nmi!();
                } else if self.irq_line.get() && !self.flags.get().contains(Flags::I) {
                    irq!();
                }

                self.active_pc.set(self.pc.get());
                let opcode = next_pc!();

                // NOTE: I believe this must be done one **whole** instruction later. Not sure.
                if let Some(state) = self.delay_intr_flag.take() {
                    self.set_flag(Flags::I, state);
                }

                include!(concat!(env!("OUT_DIR"), "/out.rs"));
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
                CoroutineState::Yielded(CpuTick { rw, addr }) => match rw {
                    Rw::Read => {
                        cpu.io_bus.set(ram[addr as usize]);
                        if addr == STDIN_ADDR {
                            // program is waiting for input, so it stopped; successfully or not
                            if stdout.contains("All tests completed, press R to repeat") {
                                break;
                            }
                            panic!("Tests failed.\nCpu state: {cpu:?}\nProgram output was:{stdout}")
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
                            let pins = Pins::from_bits_truncate(ram[FEEDBACK_ADDR as usize]);
                            let changed_pins = pins ^ feedback_register;
                            if changed_pins.contains(Pins::IRQ) {
                                cpu.irq_line.set(pins.contains(Pins::IRQ));
                            }
                            if changed_pins.contains(Pins::NMI) {
                                cpu.set_nmi(pins.contains(Pins::NMI));
                            }
                            feedback_register = pins;
                        }
                    }
                },
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
