#![allow(non_camel_case_types)]

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
                        yield_complete!(Absolute::read(&adc, self, mem_map));
                    },
                    OpCode(Op::ASL, AddressingMode::Absolute) => {
                        yield_complete!(Absolute::modify(&asl, self, mem_map));
                    },
                    OpCode(Op::CLC, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&clc, self, mem_map));
                    },
                    OpCode(Op::CLD, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&cld, self, mem_map));
                    },
                    OpCode(Op::CLI, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&cli, self, mem_map));
                    },
                    OpCode(Op::CLV, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&clv, self, mem_map));
                    },
                    OpCode(Op::LDX, AddressingMode::Absolute) => {
                        yield_complete!(Absolute::read(&ldx, self, mem_map));
                    },
                    OpCode(Op::LDX, AddressingMode::Immediate) => {
                        yield_complete!(Immediate::read(&ldx, self, mem_map));
                    },
                    OpCode(Op::NOP, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&nop, self, mem_map));
                    },
                    OpCode(Op::JMP, AddressingMode::Absolute) => {
                        yield_complete!(Absolute::jmp(self, mem_map));
                    },
                    OpCode(Op::STA, AddressingMode::Absolute) => {
                        yield_complete!(Absolute::write(&sta, self, mem_map));
                    },
                    OpCode(Op::SEC, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&sec, self, mem_map));
                    },
                    OpCode(Op::SED, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&sed, self, mem_map));
                    },
                    OpCode(Op::SEI, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&sei, self, mem_map));
                    },
                    OpCode(Op::TSX, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&tsx, self, mem_map));
                    },
                    OpCode(Op::TXS, AddressingMode::Implicit) => {
                        yield_complete!(Implicit::run(&txs, self, mem_map));
                    },
                    _ => {
                        println!("{:?} (0x{:02X?}) not implemented", instr, opcode);
                        unimplemented!();
                    }
                }

                yield CpuCycle::Done { op: instr.0, mode: instr.1 };
            }
        }
    }

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
//    fn compare(&mut self, a: u8, b: u8) {
//        let result = a - b;
//        self.flags.set(Flags::C, a >= b);
//        self.flags.set(Flags::Z, a == b);
//        self.flags.set(Flags::N, (result & 0x80) > 0);
//    }
//
//    // http://obelisk.me.uk/6502/reference.html#CMP
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
// Read operations
trait ReadOperation {
    fn operate(&self, cpu: &Cpu, value: u8);
}

// http://obelisk.me.uk/6502/reference.html#ADC
struct adc;
impl ReadOperation for adc {
    fn operate(&self, cpu: &Cpu, v: u8) {
        let acc = cpu.acc.get();
        let (v1, o1) = acc.overflowing_add(v);
        let (v2, o2) = v1.overflowing_add(cpu.flag(Flags::C) as u8);

        cpu.set_flag(Flags::C, o1 | o2);
        cpu.set_flag(Flags::V, (v^v2) & (acc^v2) & 0x80 != 0);
        cpu.acc.set(v2);
        cpu.set_flags_from_acc();
    }
}

// http://obelisk.me.uk/6502/reference.html#LDX
struct ldx;
impl ReadOperation for ldx {
    fn operate(&self, cpu: &Cpu, value: u8) {
        cpu.x.set(value);
        cpu.set_flags_from(value);
    }
}

// Read-modify-write operations
trait ModifyOperation {
    fn operate(&self, cpu: &Cpu, value: u8) -> u8;
}

// http://obelisk.me.uk/6502/reference.html#ASL
struct asl;
impl ModifyOperation for asl {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = v << 1;
        cpu.set_flag(Flags::C, (v & 0x80) != 0);
        cpu.set_flags_from(result);
        result
    }
}

// Write operations
trait WriteOperation {
    fn operate(&self, cpu: &Cpu) -> u8;
}

// http://obelisk.me.uk/6502/reference.html#STA
struct sta;
impl WriteOperation for sta {
    fn operate(&self, cpu: &Cpu) -> u8 {
        cpu.acc.get()
    }
}

trait ImplicitOperation {
    fn operate(&self, cpu: &Cpu);
}

// http://obelisk.me.uk/6502/reference.html#CLC
struct clc;
impl ImplicitOperation for clc {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::C, false);
    }
}

// http://obelisk.me.uk/6502/reference.html#CLD
struct cld;
impl ImplicitOperation for cld {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::D, false);
    }
}

// http://obelisk.me.uk/6502/reference.html#CLI
struct cli;
impl ImplicitOperation for cli {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::I, false);
    }
}

// http://obelisk.me.uk/6502/reference.html#CLV
struct clv;
impl ImplicitOperation for clv {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::V, false);
    }
}

// http://obelisk.me.uk/6502/reference.html#NOP
struct nop;
impl ImplicitOperation for nop {
    fn operate(&self, cpu: &Cpu) {

    }
}

// http://obelisk.me.uk/6502/reference.html#SEC
struct sec;
impl ImplicitOperation for sec {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::C, true);
    }
}

// http://obelisk.me.uk/6502/reference.html#SED
struct sed;
impl ImplicitOperation for sed {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::D, true);
    }
}

// http://obelisk.me.uk/6502/reference.html#SEI
struct sei;
impl ImplicitOperation for sei {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::I, true);
    }
}

// http://obelisk.me.uk/6502/reference.html#TSX
struct tsx;
impl ImplicitOperation for tsx {
    fn operate(&self, cpu: &Cpu) {
        let sp = cpu.sp.get();
        cpu.x.set(sp);
        cpu.set_flags_from(sp);
    }
}

// http://obelisk.me.uk/6502/reference.html#TXS
struct txs;
impl ImplicitOperation for txs {
    fn operate(&self, cpu: &Cpu) {
        let sp = cpu.x.get();
        cpu.sp.set(sp);
    }
}

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

    //  #  address R/W description
    // --- ------- --- -------------------------------------------------
    //  1    PC     R  fetch opcode, increment PC
    //  2    PC     R  fetch low address byte, increment PC
    //  3    PC     R  copy low address byte to PCL, fetch high address
    //                       byte to PCH
    fn jmp<'a>(cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let addr = yield_complete!(Absolute::abs_addr(cpu, mem_map));
            cpu.pc.set(addr);
        }
    }

    //  #  address R/W description
    // --- ------- --- ------------------------------------------
    //  1    PC     R  fetch opcode, increment PC
    //  2    PC     R  fetch low byte of address, increment PC
    //  3    PC     R  fetch high byte of address, increment PC
    //  4  address  R  read from effective address
    fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let addr = yield_complete!(Absolute::abs_addr(cpu, mem_map));

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
    fn modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let addr = yield_complete!(Absolute::abs_addr(cpu, mem_map));

            let value = mem_map.read_u8(addr);
            yield CpuCycle::Tick;

            mem_map.write_u8(addr, value);
            let result = operation.operate(cpu, value);
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
    fn write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let addr = yield_complete!(Absolute::abs_addr(cpu, mem_map));

            mem_map.write_u8(addr, operation.operate(cpu));
            yield CpuCycle::Tick;
        }
    }
}

struct Immediate;
impl Immediate {

    //  #  address R/W description
    // --- ------- --- ------------------------------------------
    //  1    PC     R  fetch opcode, increment PC
    //  2    PC     R  fetch value, increment PC
    fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let addr = cpu.pc_read_u8(mem_map) as u16;
            yield CpuCycle::Tick;

            let value = mem_map.read_u8(addr);
            operation.operate(cpu, cpu.acc.get());
            yield CpuCycle::Tick;
        }
    }
}

struct Implicit;
impl Implicit {
    //  #  address R/W description
    // --- ------- --- -----------------------------------------------
    //  1    PC     R  fetch opcode, increment PC
    //  2    PC     R  read next instruction byte (and throw it away)
    fn run<'a, O: ImplicitOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
        move || {
            let _ = cpu.pc_read_u8(mem_map) as u16;
            yield CpuCycle::Tick;

            operation.operate(cpu);
            yield CpuCycle::Tick;
        }
    }
}

struct Accumulator;
impl Accumulator {
    //  #  address R/W description
    // --- ------- --- -----------------------------------------------
    //  1    PC     R  fetch opcode, increment PC
    //  2    PC     R  read next instruction byte (and throw it away)
    fn modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
        move || {
            let _ = cpu.pc_read_u8(mem_map) as u16;
            yield CpuCycle::Tick;

            let result = operation.operate(cpu, cpu.acc.get());
            cpu.acc.set(result);
            yield CpuCycle::Tick;
        }
    }
}
