use crate::memory::AddressSpace;
use bitflags::bitflags;
use std::ops::{Generator, GeneratorState};
use std::cell::Cell;
use std::pin::Pin;

mod opcode;

use opcode::OPCODES;
use opcode::OpCode;

// http://obelisk.me.uk/6502/reference.html
#[derive(Debug, Clone, Copy)]
pub enum Op {
    ADC,
    AHX,
    ANC,
    AND,
    ALR,
    ARR,
    ASL,
    AXS,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DCP,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    ISC,
    JMP,
    JSR,
    KIL,
    LAX,
    LAS,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    RLA,
    ROL,
    ROR,
    RRA,
    RTI,
    RTS,
    SAX,
    SBC,
    SEC,
    SED,
    SEI,
    SHX,
    SHY,
    SLO,
    SRE,
    STA,
    STX,
    STY,
    TAS,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    XAA,
}

#[derive(Debug, Clone, Copy)]
pub enum AddressingMode {
    Immediate, // imm

    ZeroPage, // zp $00
    ZeroPageX, // zpx $00,X
    ZeroPageY, // zpy $00,Y

    Indirect, // ind
    IndirectX, // izx ($00,X)
    IndirectY, // izy ($00),Y

    Absolute, // abs $0000
    AbsoluteX, // abs $0000,X
    AbsoluteY, // abs $0000,Y

    Accumulator, // A
    Relative, // rel

    Implicit
}

// http://wiki.nesdev.com/w/index.php/Status_flags
bitflags!(
    struct Flags: u8 {
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

// http://nesdev.com/6502_cpu.txt
// http://wiki.nesdev.com/w/index.php/CPU_ALL
pub struct Cpu {
    acc: Cell<u8>,
    x: Cell<u8>,
    y: Cell<u8>,
    flags: Cell<Flags>,
    sp: Cell<u8>,
    pc: Cell<u16>,
}

pub enum CpuCycle {
    Tick,
    Done { op: Op, mode: AddressingMode }
}

impl Cpu {
    pub fn new() -> Self {
        // http://wiki.nesdev.com/w/index.php/CPU_ALL#Power_up_state
        Cpu {
            acc: Cell::new(0),
            x: Cell::new(0),
            y: Cell::new(0),
            flags: Cell::new(Flags::from_bits_truncate(0x34)),
            sp: Cell::new(0xFD),
            pc: Cell::new(0)
        }
    }

    // Reads pc and advances by one
    fn next_pc(&self) -> u16 {
        let pc = self.pc.get();
        self.pc.set(pc.wrapping_add(1));
        pc
    }

    // reads u8 at pc and advances by one
    fn pc_read_u8(&self, mem_map: &Box<&dyn AddressSpace>) -> u8 {
        let pc = self.next_pc();
        mem_map.read_u8(pc)
    }

    pub fn run<'a>(&'a self, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = !> + 'a {

        self.pc.set( mem_map.read_u16(0xFFFC));

        move || {
            loop {
                let opcode = self.pc_read_u8(mem_map);
                yield CpuCycle::Tick;

                let instr = &OPCODES[opcode as usize];

                match instr {
                    OpCode(Op::ADC, AddressingMode::Absolute) => {
                        yield_complete!(Absolute::read(&AdcOp, self, mem_map));
                    },
                    OpCode(Op::ASL, AddressingMode::Absolute) => {
                        yield_complete!(Absolute::modify(&AslOp, self, mem_map));
                    },
                    OpCode(Op::JMP, AddressingMode::Absolute) => {
                        yield_complete!(Absolute::jmp(self, mem_map));
                    },
                    _ => unimplemented!()
                }

                yield CpuCycle::Done { op: instr.0, mode: instr.1 };
            }
        }
    }
/*
    fn tick(&mut self) {
        let opcode_value = self.mem_map.read_u8(self.pc);
        let opcode = &OPCODES[opcode_value as usize];
        self.pc = self.pc.wrapping_add(1);
        let (value, addr) = match opcode.1 {
            AddressingMode::Immediate =>
                (Some(self.mem_map.read_u8(self.pc) as u16), None),
            AddressingMode::ZeroPage => {
                let addr = self.mem_map.read_u8(self.pc) as u16;
                (Some(self.mem_map.read_u8(addr) as u16), Some(addr))
            },
            AddressingMode::ZeroPageX => {
                let addr = self.mem_map.read_u8(self.pc).wrapping_add(self.x) as u16;
                (Some(self.mem_map.read_u8(addr) as u16), Some(addr))
            },
            AddressingMode::ZeroPageY =>  {
                let addr = self.mem_map.read_u8(self.pc).wrapping_add(self.y) as u16;
                (Some(self.mem_map.read_u8(addr) as u16), Some(addr))
            },

            AddressingMode::Absolute => {
                let addr = self.mem_map.read_u16(self.pc);
                (Some(self.mem_map.read_u8(addr) as u16), Some(addr))
            },
            AddressingMode::AbsoluteX => {
                let addr = self.mem_map.read_u16(self.pc).wrapping_add(self.x as u16);
                (Some(self.mem_map.read_u8(addr) as u16), Some(addr))
            },
            AddressingMode::AbsoluteY => {
                let addr = self.mem_map.read_u16(self.pc).wrapping_add(self.y as u16);
                (Some(self.mem_map.read_u8(addr) as u16), Some(addr))
            },

            AddressingMode::Indirect => {
                // this is only used by JMP and its behaviour was actually buggy.
                //   if addr is 0x01FF, it would fetch the lsb from 0x01FF, but msb at 0x0100 instead of 0x0200
                let addr = self.mem_map.read_u16(self.pc);
                let lsb = self.mem_map.read_u8(addr) as u16;
                let msb_addr = if addr & 0xFF != 0xFF { addr.wrapping_add(1) } else { addr & 0xFF00 };
                let msb = self.mem_map.read_u8(msb_addr) as u16;
                let v = (msb << 8) | lsb;
                (Some(v), Some(addr))
            },
            AddressingMode::IndirectX => {
                // val = read_u8(read_u8((arg + X) % 256) + read_u8((arg + X + 1) % 256) * 256)
                let addr = self.mem_map.read_u8(self.pc).wrapping_add(self.x);
                let v = self.mem_map.read_u16(addr as u16);
                (Some(v), Some(addr as u16))
            },
            AddressingMode::IndirectY => {
                // val = read_u8(read_u8(arg) + read_u8((arg + 1) % 256) * 256 + Y)
                let addr = self.mem_map.read_u8(self.pc);
                let v = self.mem_map.read_u16(addr as u16).wrapping_add(self.y as u16);
                (Some(v), Some(addr as u16))
            },

            AddressingMode::Accumulator => (Some(self.acc as u16), None),
            AddressingMode::Relative => (Some(self.mem_map.read_u8(self.pc) as u16), None),

            AddressingMode::Implicit => (None, None)
        };

        match (opcode.0, value, addr) {
            (Op::ADC, Some(v), None) => self.adc(v as u8),
            (Op::AND, Some(v), None) => self.and(v as u8),

            (Op::ASL, Some(v), None) => {
                self.acc = self.asl(v as u8);
                self.set_flags_from_acc();
            },
            (Op::ASL, Some(v), Some(addr)) => {
                let result = self.asl(v as u8);
                self.mem_map.write_u8(addr, result);
                self.set_flags_from(result);
            },

            (Op::BCC, Some(v), _) => self.bcc(v as u8),
            (Op::BCS, Some(v), _) => self.bcs(v as u8),
            (Op::BEQ, Some(v), _) => self.beq(v as u8),
            (Op::BIT, Some(v), _) => self.bit(v as u8),
            (Op::BMI, Some(v), _) => self.bmi(v as u8),
            (Op::BNE, Some(v), _) => self.bne(v as u8),
            (Op::BPL, Some(v), _) => self.bpl(v as u8),
            (Op::BRK, None, None) => self.brk(),
            (Op::BVC, Some(v), _) => self.bvc(v as u8),
            (Op::BVS, Some(v), _) => self.bvs(v as u8),
            (Op::CLC, None, None) => self.clc(),
            (Op::CLD, None, None) => self.cld(),
            (Op::CLI, None, _) => self.cli(),
            (Op::CLV, None, _) => self.clv(),

            (Op::CMP, Some(v), _) => self.cmp(v as u8),
            (Op::CPX, Some(v), _) => self.cpx(v as u8),
            (Op::CPY, Some(v), _) => self.cpy(v as u8),

            (Op::DEC, Some(v), Some(addr)) => {
                let result = self.dec(v as u8);
                self.mem_map.write_u8(addr, result);
                self.set_flags_from(result);
            },
            (Op::DEX, None, _) => self.dex(),
            (Op::DEY, None, _) => self.dey(),

            (Op::EOR, Some(v), _) => self.eor(v as u8),

            (Op::INC, Some(v), Some(addr)) => {
                let result = self.inc(v as u8);
                self.mem_map.write_u8(addr, result);
                self.set_flags_from(result);
            },
            (Op::INX, None, _) => self.inx(),
            (Op::INY, None, _) => self.iny(),

            (Op::JMP, Some(addr), _) => self.jmp(addr),
            (Op::JSR, Some(v), _) => self.jsr(v),

            _ => unimplemented!()
        }

    }
*/
    // TODO
    fn push_stack(&mut self, v: u8) {
        unimplemented!()
    }
    fn push_stack16(&mut self, v: u16) {
        unimplemented!()
    }
    fn pop_stack(&mut self) -> u8 {
        unimplemented!()
    }
    fn pop_stack16(&mut self) -> u16 {
        unimplemented!()
    }

    fn flag(&self, f: Flags) -> bool {
        self.flags.get().contains(Flags::C)
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

}

// Operations
impl Cpu {

    // http://obelisk.me.uk/6502/reference.html#ADC
    fn adc(&self, v: u8) {
        let acc = self.acc.get();
        let (v1, o1) = acc.overflowing_add(v);
        let (v2, o2) = v1.overflowing_add(self.flag(Flags::C) as u8);

        self.set_flag(Flags::C, o1 | o2);
        self.set_flag(Flags::V, (v^v2) & (acc^v2) & 0x80 != 0);
        self.acc.set(v2);
        self.set_flags_from_acc();
    }

//    // http://obelisk.me.uk/6502/reference.html#AND
//    fn and(&mut self, v: u8) {
//        self.acc = self.acc & v;
//        self.set_flags_from_acc();
//    }
//
    // http://obelisk.me.uk/6502/reference.html#ASL
    fn asl(&self, v: u8) -> u8 {
        let result = v << 1;
        self.set_flag(Flags::C, (v & 0x80) != 0);
        self.set_flags_from(result);
        result
    }
//
//    fn branch_if(&mut self, branch: bool, v: u8) {
//        if branch {
//            // TODO: +1 cycle
//            // relative displacement, v is signed in this case
//            self.pc = self.pc.wrapping_add(v as i8 as u16);
//            // TODO: +1 if branching to a new page
//        }
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BCC
//    fn bcc(&mut self, v: u8) {
//        self.branch_if(!self.flags.contains(Flags::C), v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BCS
//    fn bcs(&mut self, v: u8) {
//        self.branch_if(self.flags.contains(Flags::C), v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BEQ
//    fn beq(&mut self, v: u8) {
//        self.branch_if(self.flags.contains(Flags::Z), v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BIT
//    fn bit(&mut self, v: u8) {
//        let r = self.acc & v;
//        self.flags.set(Flags::Z, r == 0);
//        self.flags.set(Flags::V, (v & 0x40) != 0); // set to the 6th bit of the value
//        self.flags.set(Flags::N, (v & 0x80) != 0); // set to the 7th bit of the value
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BMI
//    fn bmi(&mut self, v: u8) {
//        self.branch_if(self.flags.contains(Flags::N), v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BNE
//    fn bne(&mut self, v: u8) {
//        self.branch_if(!self.flags.contains(Flags::Z), v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BPL
//    fn bpl(&mut self, v: u8) {
//        self.branch_if(!self.flags.contains(Flags::N), v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BRK
//    fn brk(&mut self) {
//        self.push_stack16(self.pc);
//        self.push_stack(self.flags.bits);
////        self.pc = self.mem_map.read_u16(0xFFFE);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BVC
//    fn bvc(&mut self, v: u8) {
//        self.branch_if(!self.flags.contains(Flags::V), v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#BVS
//    fn bvs(&mut self, v: u8) {
//        self.branch_if(self.flags.contains(Flags::V), v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#CLC
//    fn clc(&mut self) {
//        self.flags.set(Flags::C, false)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#CLD
//    fn cld(&mut self) {
//        self.flags.set(Flags::D, false)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#CLI
//    fn cli(&mut self) {
//        self.flags.set(Flags::I, false)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#CLV
//    fn clv(&mut self) {
//        self.flags.set(Flags::V, false)
//    }
//
//    fn compare(&mut self, a: u8, b: u8) {
//        let result = a - b;
//        self.flags.set(Flags::C, a >= b);
//        self.flags.set(Flags::Z, a == b);
//        self.flags.set(Flags::N, (result & 0x80) > 0);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#CLV
//    fn cmp(&mut self, v: u8) {
//        self.compare(self.acc, v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#CPX
//    fn cpx(&mut self, v: u8) {
//        self.compare(self.x, v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#CPY
//    fn cpy(&mut self, v: u8) {
//        self.compare(self.y, v)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#DEC
//    fn dec(&mut self, v: u8) -> u8 {
//        let result = v.wrapping_sub(1);
//        self.set_flags_from(result);
//        result
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#DEX
//    fn dex(&mut self) {
//        self.x = self.dec(self.x);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#DEY
//    fn dey(&mut self) {
//        self.y = self.dec(self.y);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#EOR
//    fn eor(&mut self, v: u8) {
//        self.acc ^= v;
//        self.set_flags_from_acc();
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#INC
//    fn inc(&mut self, v: u8) -> u8 {
//        let result = v.wrapping_add(1);
//        self.set_flags_from(result);
//        result
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#INX
//    fn inx(&mut self) {
//        self.x = self.inc(self.x);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#INY
//    fn iny(&mut self) {
//        self.y = self.inc(self.y);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#JMP
//    fn jmp(&mut self, v: u16) {
//        self.pc = v;
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#JSR
//    fn jsr(&mut self, v: u16) {
//        self.push_stack16(self.pc.wrapping_sub(1));
//        self.jmp(v);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#LDA
//    fn lda(&mut self, v: u8) {
//        self.acc = v;
//        self.set_flags_from_acc();
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#LDX
//    fn ldx(&mut self, v: u8) {
//        self.x = v;
//        self.set_flags_from(self.x);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#LDY
//    fn ldy(&mut self, v: u8) {
//        self.y = v;
//        self.set_flags_from(self.y);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#LSR
//    fn lsr(&mut self, v: u8) -> u8 {
//        self.flags.set(Flags::C, (v & 0x01) != 0);
//        let result = v >> 1;
//        self.set_flags_from(result);
//        result
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#NOP
//    fn nop(&mut self) {
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#ORA
//    fn ora(&mut self, v: u8) {
//        self.acc |= v;
//        self.set_flags_from_acc();
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#PHA
//    fn pha(&mut self) {
//        self.push_stack(self.flags.bits());
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#PLA
//    fn pla(&mut self) {
//        self.acc = self.pop_stack();
//        self.set_flags_from_acc();
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#PLP
//    fn plp(&mut self) {
//        self.flags = Flags::from_bits_truncate(self.pop_stack());
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#ROL
//    fn rol(&mut self, v: u8) -> u8 {
//        let result = (v << 1) | self.flags.contains(Flags::C) as u8;
//        self.flags.set(Flags::C, v & 0x80 != 0);
//        self.set_flags_from(result);
//        result
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#ROR
//    fn ror(&mut self, v: u8) -> u8 {
//        let result = (v >> 1) | ((self.flags.contains(Flags::C) as u8) << 7);
//        self.flags.set(Flags::C, v & 0x01 != 0);
//        self.set_flags_from(result);
//        result
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#RTI
//    fn rti(&mut self) {
//        self.flags = Flags::from_bits_truncate(self.pop_stack());
//        self.pc = self.pop_stack16();
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#RTS
//    fn rts(&mut self) {
//        self.pc = self.pop_stack16().wrapping_add(1);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#SBC
//    fn sbc(&mut self, v: u8) {
//        let (v1, o1) = self.acc.overflowing_sub(v);
//        let (v2, o2) = v1.overflowing_sub(!self.flags.contains(Flags::C) as u8);
//        self.flags.set(Flags::C, o1 | o2);
//        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
//        self.flags.set(Flags::V, (v^v2) & (self.acc^v2) & 0x80 != 0);
//        self.acc = v2;
//        self.set_flags_from_acc();
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#SEC
//    fn sec(&mut self) {
//        self.flags.set(Flags::C, true)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#SED
//    fn sed(&mut self) {
//        self.flags.set(Flags::D, true)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#SEI
//    fn sei(&mut self) {
//        self.flags.set(Flags::I, true)
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#STA
//    fn sta(&mut self, v: u16) {
////        self.mem_map.write_u8(v, self.acc);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#STX
//    fn stx(&mut self, v: u16) {
////        self.mem_map.write_u8(v, self.x);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#STY
//    fn sty(&mut self, v: u16) {
////        self.mem_map.write_u8(v, self.y);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#TAX
//    fn tax(&mut self) {
//        self.x = self.acc;
//        self.set_flags_from(self.x);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#TAY
//    fn tay(&mut self) {
//        self.y = self.acc;
//        self.set_flags_from(self.y);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#TSX
//    fn tsx(&mut self) {
//        self.x = self.sp;
//        self.set_flags_from(self.x);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#TXA
//    fn txa(&mut self) {
//        self.acc = self.x;
//        self.set_flags_from_acc();
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#TXS
//    fn txs(&mut self) {
//        self.sp = self.x;
//        self.set_flags_from_acc();
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#TYA
//    fn tya(&mut self) {
//        self.acc = self.y;
//        self.set_flags_from_acc();
//    }

}

// http://nesdev.com/6502_cpu.txt
trait ReadOperation {
    fn op(&self) -> Op;
    fn operate(&self, cpu: &Cpu, value: u8);
}

struct AdcOp;
impl ReadOperation for AdcOp {
    fn op(&self) -> Op {
        Op::ADC
    }

    fn operate(&self, cpu: &Cpu, value: u8) {
        cpu.adc(value)
    }
}

trait ModifyOperation {
    fn op(&self) -> Op;
    fn operate(&self, cpu: &Cpu, value: u8) -> u8;
}
struct AslOp;
impl ModifyOperation for AslOp {
    fn op(&self) -> Op {
        Op::ASL
    }

    fn operate(&self, cpu: &Cpu, value: u8) -> u8 {
        cpu.asl(value)
    }
}

// TODO: Use this type alias leads to a compiler error
//type Gn<'a> = impl Generator<Yield = CpuCycle, Return = CpuCycle> + 'a;
struct Absolute;
impl Absolute {

    fn abs_addr<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = u16> + 'a {
        move || {
            let addr_lo = cpu.pc_read_u8(mem_map) as u16;
            yield CpuCycle::Tick;

            let addr_hi = cpu.pc_read_u8(mem_map) as u16;
            yield CpuCycle::Tick;

            addr_hi << 8 | addr_lo
        }
    }

    // #  address R/W description
    //       --- ------- --- -------------------------------------------------
    //        1    PC     R  fetch opcode, increment PC
    //        2    PC     R  fetch low address byte, increment PC
    //        3    PC     R  copy low address byte to PCL, fetch high address
    //                       byte to PCH
    fn jmp<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let addr = yield_complete!(Absolute::abs_addr(cpu, mem_map));
            cpu.pc.set(addr);
        }
    }

    // #  address R/W description
    //       --- ------- --- ------------------------------------------
    //        1    PC     R  fetch opcode, increment PC
    //        2    PC     R  fetch low byte of address, increment PC
    //        3    PC     R  fetch high byte of address, increment PC
    //        4  address  R  read from effective address
    fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let addr = yield_complete!(Absolute::abs_addr(cpu, mem_map));

            let value = mem_map.read_u8(addr);
            operation.operate(cpu, value);
            yield CpuCycle::Tick;
        }
    }

    // #  address R/W description
    //       --- ------- --- ------------------------------------------
    //        1    PC     R  fetch opcode, increment PC
    //        2    PC     R  fetch low byte of address, increment PC
    //        3    PC     R  fetch high byte of address, increment PC
    //        4  address  R  read from effective address
    //        5  address  W  write the value back to effective address,
    //                       and do the operation on it
    //        6  address  W  write the new value to effective address
    fn modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let addr = yield_complete!(Absolute::abs_addr(cpu, mem_map));

            let value = mem_map.read_u8(addr);
            yield CpuCycle::Tick;

            mem_map.write_u8(addr, value);
            let result = operation.operate(cpu, value);
            yield CpuCycle::Tick;

            mem_map.write_u8(addr, result);
        }
    }
}
