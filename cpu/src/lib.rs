#![feature(coroutines, coroutine_trait)]
use std::{cell::Cell, ops::Coroutine};

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
    Imp, // Implicit
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

struct NmiLine {
    // state of the NMI line
    line: bool,
    // internal flip-flop that stores edge detection on the line
    state: bool,
}

impl Default for NmiLine {
    fn default() -> Self {
        Self {
            line: false,
            state: false,
        }
    }
}

pub enum Rw {
    Read,
    Write,
}
pub struct CpuTick {
    rw: Rw,
    addr: u16,
}

pub struct Cpu {
    acc: Cell<u8>,
    x: Cell<u8>,
    y: Cell<u8>,
    flags: Cell<Flags>,
    sp: Cell<u8>,
    pc: Cell<u16>,

    // /NMI line - edge sensitive - high to low
    nmi_line: Cell<NmiLine>,
    // IRQ line - level sensitive
    irq_line: Cell<bool>,
    // /RESET line - held low to reset CPU
    rst_line: Cell<bool>,

    io_bus: Cell<u8>,
}

impl Default for Cpu {
    fn default() -> Self {
        Cpu::new()
    }
}

impl Cpu {
    // http://wiki.nesdev.com/w/index.php/CPU_ALL#Power_up_state
    fn new() -> Self {
        Cpu {
            acc: Cell::new(0),
            x: Cell::new(0),
            y: Cell::new(0),
            flags: Cell::new(Flags::I),
            sp: Cell::new(0xFD),
            pc: Cell::new(0),

            nmi_line: Cell::default(),
            irq_line: Cell::new(false),
            rst_line: Cell::new(false),

            io_bus: Cell::new(0),
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
        macro_rules! read {
            ( $addr: expr ) => {{
                yield CpuTick {
                    rw: Rw::Read,
                    addr: $addr,
                };
                self.io_bus.get()
            }};
        }
        macro_rules! write {
            ( $addr: expr, $value: expr ) => {{
                self.io_bus.set($value);
                yield CpuTick {
                    rw: Rw::Write,
                    addr: $addr,
                };
            }};
        }
        macro_rules! next_pc {
            () => {{
                let value = read!(self.pc.get());
                self.pc.update(|pc| pc.wrapping_add(1));
                value
            }};
        }
        macro_rules! push {
            ($value:expr) => {
                write!(0x0100 | (self.sp.get() as u16), $value);
                self.sp.update(|sp| sp.wrapping_sub(1));
            };
        }
        macro_rules! pop {
            ($value:expr) => {
                self.sp.update(|sp| sp.wrapping_add(1));
                read!(0x0100 | (self.sp.get() as u16))
            };
        }

        #[coroutine]
        move || {
            loop {
                let OpCode(op, mode) = OpCode::decode(next_pc!());
                macro_rules! zp_indexed {
                    ( $index: expr ) => {{
                        let addr = next_pc!();
                        read!(addr as u16);
                        let addr = addr.wrapping_add($index) as u16;
                        let value = read!(addr);

                        match op.implementation() {
                            OpImpl::Read(r) => {
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
                                r.operate(self, value);
                            }
                            OpImpl::Modify(m) => {
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
                                write!(addr, value);
                                let (_, value) = m.modify(self, addr, value);
                                write!(addr, value);
                            }
                            OpImpl::Write(w) => {
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
                                //          i.e. page boundary crossings are not handled.}
                                write!(addr, w.operate(self));
                            }
                            _ => panic!("operation {:?} not supported in Zero Page X mode", op),
                        }
                    }};
                }

                match mode {
                    AddressingMode::Imp => todo!(),
                    AddressingMode::Acc => {
                        //  #  address R/W description
                        // --- ------- --- -----------------------------------------------
                        //  1    PC     R  fetch opcode, increment PC
                        //  2    PC     R  read next instruction byte (and throw it away)
                        read!(self.pc.get());
                        match op.implementation() {
                            OpImpl::Modify(m) => {
                                self.acc.update(|acc| m.operate(self, acc));
                            }
                            _ => panic!("operation {:?} not supported in Accumulator mode", op),
                        }
                    }
                    AddressingMode::Imm => todo!(),
                    AddressingMode::Zp0 => {
                        let addr = next_pc!() as u16;
                        let value = read!(addr);
                        match op.implementation() {
                            OpImpl::Read(r) => {
                                //  #   address  R/W description
                                // --- --------- --- ------------------------------------------
                                //  1     PC      R  fetch opcode, increment PC
                                //  2     PC      R  fetch address, increment PC
                                //  3   address   R  read from effective address
                                r.operate(self, value);
                            }
                            OpImpl::Modify(m) => {
                                //  #  address R/W description
                                // --- ------- --- ------------------------------------------
                                //  1    PC     R  fetch opcode, increment PC
                                //  2    PC     R  fetch address, increment PC
                                //  3  address  R  read from effective address
                                //  4  address  W  write the value back to effective address,
                                //                 and do the operation on it
                                //  5  address  W  write the new value to effective address
                                write!(addr, value);
                                let (_, result) = m.modify(self, addr, value);
                                write!(addr, result);
                            }
                            OpImpl::Write(w) => {
                                //  #  address R/W description
                                // --- ------- --- ------------------------------------------
                                //  1    PC     R  fetch opcode, increment PC
                                //  2    PC     R  fetch address, increment PC
                                //  3  address  W  write register to effective address
                                write!(addr, w.operate(self));
                            }
                            _ => panic!("operation {:?} not supported in Zero Page mode", op),
                        }
                    }
                    AddressingMode::ZpX => zp_indexed!(self.x.get()),
                    AddressingMode::ZpY => zp_indexed!(self.y.get()),
                    AddressingMode::Rel => todo!(),
                    AddressingMode::Abs => todo!(),
                    AddressingMode::AbX => todo!(),
                    AddressingMode::AbY => todo!(),
                    AddressingMode::Ind => todo!(),
                    AddressingMode::IdX => todo!(),
                    AddressingMode::IdY => todo!(),
                }
            }
        }
    }
}

trait BranchOperation {
    fn branch(&self, cpu: &Cpu) -> bool;
}

trait ImplicitOperation {
    fn operate(&self, cpu: &Cpu);
}

trait ModifyOperation {
    // this includes addr because TAS, SHX and SHY operate on the high byte of the target address
    //   instead of the value
    fn modify(&self, cpu: &Cpu, addr: u16, value: u8) -> (u16, u8) {
        (addr, self.operate(cpu, value))
    }
    fn operate(&self, cpu: &Cpu, value: u8) -> u8;
}

trait ReadOperation {
    fn operate(&self, cpu: &Cpu, value: u8);
}

trait WriteOperation {
    fn operate(&self, cpu: &Cpu) -> u8;
}

enum OpImpl {
    Read(&'static dyn ReadOperation),
    Modify(&'static dyn ModifyOperation),
    Write(&'static dyn WriteOperation),
    Implicit(&'static dyn ImplicitOperation),
}

impl Op {
    fn implementation(&self) -> OpImpl {
        use Op::*;
        use operations::*;

        match self {
            ADC => OpImpl::Read(&adc),
            AND => OpImpl::Read(&and),
            ASL => OpImpl::Modify(&asl),
            LDA => OpImpl::Read(&lda),
            STA => OpImpl::Write(&sta),
            TAX => OpImpl::Implicit(&tax),
            TAY => OpImpl::Implicit(&tay),
            _ => unimplemented!(),
        }
    }
}
