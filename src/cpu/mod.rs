#![allow(non_camel_case_types)]

use crate::memory::AddressSpace;
use bitflags::bitflags;
use std::ops::Generator;
use std::cell::Cell;

mod absolute;
mod absolute_indexed;
mod indirect;
mod indirect_indexed;
mod opcode;
mod stack;
mod zero_page;
mod zero_page_indexed;

pub mod generator;

use opcode::OPCODES;
use opcode::OpCode;
use std::fmt::{Display, Formatter, Error};

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

impl Cpu {
    // TODO: move mem_map as a member to Cpu to allow this within Display
    pub fn write(&self, mem_map: &Box<&dyn AddressSpace>) -> String{
        let pc = self.pc.get();
        let addr = mem_map.read_u8(pc);
        let OpCode(op, _) = &OPCODES[addr as usize];
        format!("{:02X?} {:?} {}", pc, op, self)
    }
}

impl Display for Cpu {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "A:{:02X?} X:{:02X?} Y:{:02X?} P:{:02X?} ({:?}) SP:{:02X?}", self.acc.get(), self.x.get(), self.y.get(), self.flags.get().bits(), self.flags.get(), self.sp.get())
    }
}

pub enum CpuCycle {
    Tick,
    Done { pc: u16, op: Op, mode: AddressingMode }
}

impl Cpu {
    pub fn new() -> Self {
        // http://wiki.nesdev.com/w/index.php/CPU_ALL#Power_up_state
        Cpu {
            acc: Cell::new(0),
            x: Cell::new(0),
            y: Cell::new(0),
            flags: Cell::new(Flags::from_bits_truncate(0x24)), // The wiki says 0x34, but nestest seems to use 0x24
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
    fn pc_read_u8_next(&self, mem_map: &Box<&dyn AddressSpace>) -> u8 {
        let pc = self.next_pc();
        mem_map.read_u8(pc)
    }

    // reads u8 at pc
    fn pc_read_u8(&self, mem_map: &Box<&dyn AddressSpace>) -> u8 {
        let pc = self.pc.get();
        mem_map.read_u8(pc)
    }

    pub fn run<'a>(&'a self, mem_map: &'a Box<&dyn AddressSpace>, start_at: Option<u16>) -> impl Generator<Yield = CpuCycle, Return = !> + 'a {

        let pc = start_at.unwrap_or_else(|| mem_map.read_u16(0xFFFC));
        self.pc.set(pc);

        move || {
            loop {
                let this_pc = self.pc.get();
                let opcode = self.pc_read_u8_next(mem_map);
                yield CpuCycle::Tick;

                // TODO
                let instr = &OPCODES[opcode as usize];

                match opcode {
                    0x01 => yield_complete!(indirect_indexed::x_read(&ora, self, mem_map)),
                    0x04 => yield_complete!(zero_page::read(&nop, self, mem_map)),
                    0x05 => yield_complete!(zero_page::read(&ora, self, mem_map)),
                    0x06 => yield_complete!(zero_page::modify(&asl, self, mem_map)),
                    0x08 => yield_complete!(stack::php(self, mem_map)),
                    0x09 => yield_complete!(immediate::read(&ora, self, mem_map)),
                    0x0A => yield_complete!(accumulator::modify(&asl, self, mem_map)),
                    0x0C => yield_complete!(absolute::read(&nop, self, mem_map)),
                    0x0D => yield_complete!(absolute::read(&ora, self, mem_map)),
                    0x0E => yield_complete!(absolute::modify(&asl, self, mem_map)),
                    0x10 => yield_complete!(relative::branch(&bpl, self, mem_map)),
                    0x11 => yield_complete!(indirect_indexed::y_read(&ora, self, mem_map)),
                    0x14 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0x15 => yield_complete!(zero_page_indexed::x_read(&ora, self, mem_map)),
                    0x16 => yield_complete!(zero_page_indexed::x_modify(&asl, self, mem_map)),
                    0x18 => yield_complete!(implicit::run(&clc, self, mem_map)),
                    0x19 => yield_complete!(absolute_indexed::y_read(&ora, self, mem_map)),
                    0x1A => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0x1C => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0x1D => yield_complete!(absolute_indexed::x_read(&ora, self, mem_map)),
                    0x1E => yield_complete!(absolute_indexed::x_modify(&asl, self, mem_map)),
                    0x20 => yield_complete!(stack::jsr(self, mem_map)),
                    0x21 => yield_complete!(indirect_indexed::x_read(&and, self, mem_map)),
                    0x24 => yield_complete!(zero_page::read(&bit, self, mem_map)),
                    0x25 => yield_complete!(zero_page::read(&and, self, mem_map)),
                    0x26 => yield_complete!(zero_page::modify(&rol, self, mem_map)),
                    0x28 => yield_complete!(stack::plp(self, mem_map)),
                    0x29 => yield_complete!(immediate::read(&and, self, mem_map)),
                    0x2A => yield_complete!(accumulator::modify(&rol, self, mem_map)),
                    0x2C => yield_complete!(absolute::read(&bit, self, mem_map)),
                    0x2D => yield_complete!(absolute::read(&and, self, mem_map)),
                    0x2E => yield_complete!(absolute::modify(&rol, self, mem_map)),
                    0x30 => yield_complete!(relative::branch(&bmi, self, mem_map)),
                    0x31 => yield_complete!(indirect_indexed::y_read(&and, self, mem_map)),
                    0x34 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0x35 => yield_complete!(zero_page_indexed::x_read(&and, self, mem_map)),
                    0x36 => yield_complete!(zero_page_indexed::x_modify(&rol, self, mem_map)),
                    0x38 => yield_complete!(implicit::run(&sec, self, mem_map)),
                    0x39 => yield_complete!(absolute_indexed::y_read(&and, self, mem_map)),
                    0x3A => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0x3C => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0x3D => yield_complete!(absolute_indexed::x_read(&and, self, mem_map)),
                    0x3E => yield_complete!(absolute_indexed::x_modify(&rol, self, mem_map)),
                    0x40 => yield_complete!(stack::rti(self, mem_map)),
                    0x41 => yield_complete!(indirect_indexed::x_read(&eor, self, mem_map)),
                    0x44 => yield_complete!(zero_page::read(&nop, self, mem_map)),
                    0x45 => yield_complete!(zero_page::read(&eor, self, mem_map)),
                    0x46 => yield_complete!(zero_page::modify(&lsr, self, mem_map)),
                    0x48 => yield_complete!(stack::pha(self, mem_map)),
                    0x49 => yield_complete!(immediate::read(&eor, self, mem_map)),
                    0x4A => yield_complete!(accumulator::modify(&lsr, self, mem_map)),
                    0x4C => yield_complete!(absolute::jmp(self, mem_map)),
                    0x4D => yield_complete!(absolute::read(&eor, self, mem_map)),
                    0x4E => yield_complete!(absolute::modify(&lsr, self, mem_map)),
                    0x50 => yield_complete!(relative::branch(&bvc, self, mem_map)),
                    0x51 => yield_complete!(indirect_indexed::y_read(&eor, self, mem_map)),
                    0x54 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0x55 => yield_complete!(zero_page_indexed::x_read(&eor, self, mem_map)),
                    0x56 => yield_complete!(zero_page_indexed::x_modify(&lsr, self, mem_map)),
                    0x58 => yield_complete!(implicit::run(&cli, self, mem_map)),
                    0x59 => yield_complete!(absolute_indexed::y_read(&eor, self, mem_map)),
                    0x5A => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0x5C => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0x5D => yield_complete!(absolute_indexed::x_read(&eor, self, mem_map)),
                    0x5E => yield_complete!(absolute_indexed::x_modify(&lsr, self, mem_map)),
                    0x60 => yield_complete!(stack::rts(self, mem_map)),
                    0x61 => yield_complete!(indirect_indexed::x_read(&adc, self, mem_map)),
                    0x64 => yield_complete!(zero_page::read(&nop, self, mem_map)),
                    0x65 => yield_complete!(zero_page::read(&adc, self, mem_map)),
                    0x66 => yield_complete!(zero_page::modify(&ror, self, mem_map)),
                    0x68 => yield_complete!(stack::pla(self, mem_map)),
                    0x69 => yield_complete!(immediate::read(&adc, self, mem_map)),
                    0x6A => yield_complete!(accumulator::modify(&ror, self, mem_map)),
                    0x6C => yield_complete!(absolute::jmp(self, mem_map)),
                    0x6D => yield_complete!(absolute::read(&adc, self, mem_map)),
                    0x6E => yield_complete!(absolute::modify(&ror, self, mem_map)),
                    0x70 => yield_complete!(relative::branch(&bvs, self, mem_map)),
                    0x71 => yield_complete!(indirect_indexed::y_read(&adc, self, mem_map)),
                    0x74 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0x75 => yield_complete!(zero_page_indexed::x_read(&adc, self, mem_map)),
                    0x76 => yield_complete!(zero_page_indexed::x_modify(&ror, self, mem_map)),
                    0x78 => yield_complete!(implicit::run(&sei, self, mem_map)),
                    0x79 => yield_complete!(absolute_indexed::y_read(&adc, self, mem_map)),
                    0x7A => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0x7C => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0x7D => yield_complete!(absolute_indexed::x_read(&adc, self, mem_map)),
                    0x7E => yield_complete!(absolute_indexed::x_modify(&ror, self, mem_map)),
                    0x80 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0x81 => yield_complete!(indirect_indexed::x_write(&sta, self, mem_map)),
                    0x82 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0x84 => yield_complete!(zero_page::write(&sty, self, mem_map)),
                    0x85 => yield_complete!(zero_page::write(&sta, self, mem_map)),
                    0x86 => yield_complete!(zero_page::write(&stx, self, mem_map)),
                    0x88 => yield_complete!(implicit::run(&dey, self, mem_map)),
                    0x89 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0x8A => yield_complete!(implicit::run(&txa, self, mem_map)),
                    0x8C => yield_complete!(absolute::write(&sty, self, mem_map)),
                    0x8D => yield_complete!(absolute::write(&sta, self, mem_map)),
                    0x8E => yield_complete!(absolute::write(&stx, self, mem_map)),
                    0x90 => yield_complete!(relative::branch(&bcc, self, mem_map)),
                    0x91 => yield_complete!(indirect_indexed::y_write(&sta, self, mem_map)),
                    0x94 => yield_complete!(zero_page_indexed::x_write(&sty, self, mem_map)),
                    0x95 => yield_complete!(zero_page_indexed::x_write(&sta, self, mem_map)),
                    0x96 => yield_complete!(zero_page_indexed::y_write(&stx, self, mem_map)),
                    0x98 => yield_complete!(implicit::run(&tya, self, mem_map)),
                    0x99 => yield_complete!(absolute_indexed::y_write(&sta, self, mem_map)),
                    0x9A => yield_complete!(implicit::run(&txs, self, mem_map)),
                    0x9D => yield_complete!(absolute_indexed::x_write(&sta, self, mem_map)),
                    0xA0 => yield_complete!(immediate::read(&ldy, self, mem_map)),
                    0xA1 => yield_complete!(indirect_indexed::x_read(&lda, self, mem_map)),
                    0xA2 => yield_complete!(immediate::read(&ldx, self, mem_map)),
                    0xA4 => yield_complete!(zero_page::read(&ldy, self, mem_map)),
                    0xA5 => yield_complete!(zero_page::read(&lda, self, mem_map)),
                    0xA6 => yield_complete!(zero_page::read(&ldx, self, mem_map)),
                    0xA8 => yield_complete!(implicit::run(&tay, self, mem_map)),
                    0xA9 => yield_complete!(immediate::read(&lda, self, mem_map)),
                    0xAA => yield_complete!(implicit::run(&tax, self, mem_map)),
                    0xAC => yield_complete!(absolute::read(&ldy, self, mem_map)),
                    0xAD => yield_complete!(absolute::read(&lda, self, mem_map)),
                    0xAE => yield_complete!(absolute::read(&ldx, self, mem_map)),
                    0xB0 => yield_complete!(relative::branch(&bcs, self, mem_map)),
                    0xB1 => yield_complete!(indirect_indexed::y_read(&lda, self, mem_map)),
                    0xB4 => yield_complete!(zero_page_indexed::x_read(&ldy, self, mem_map)),
                    0xB5 => yield_complete!(zero_page_indexed::x_read(&lda, self, mem_map)),
                    0xB6 => yield_complete!(zero_page_indexed::y_read(&ldx, self, mem_map)),
                    0xB8 => yield_complete!(implicit::run(&clv, self, mem_map)),
                    0xB9 => yield_complete!(absolute_indexed::y_read(&lda, self, mem_map)),
                    0xBA => yield_complete!(implicit::run(&tsx, self, mem_map)),
                    0xBC => yield_complete!(absolute_indexed::x_read(&ldy, self, mem_map)),
                    0xBD => yield_complete!(absolute_indexed::x_read(&lda, self, mem_map)),
                    0xBE => yield_complete!(absolute_indexed::y_read(&ldx, self, mem_map)),
                    0xC0 => yield_complete!(immediate::read(&cpy, self, mem_map)),
                    0xC1 => yield_complete!(indirect_indexed::x_read(&cmp, self, mem_map)),
                    0xC2 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0xC4 => yield_complete!(zero_page::read(&cpy, self, mem_map)),
                    0xC5 => yield_complete!(zero_page::read(&cmp, self, mem_map)),
                    0xC6 => yield_complete!(zero_page::modify(&dec, self, mem_map)),
                    0xC8 => yield_complete!(implicit::run(&iny, self, mem_map)),
                    0xC9 => yield_complete!(immediate::read(&cmp, self, mem_map)),
                    0xCA => yield_complete!(implicit::run(&dex, self, mem_map)),
                    0xCC => yield_complete!(absolute::read(&cpy, self, mem_map)),
                    0xCD => yield_complete!(absolute::read(&cmp, self, mem_map)),
                    0xCE => yield_complete!(absolute::modify(&dec, self, mem_map)),
                    0xD0 => yield_complete!(relative::branch(&bne, self, mem_map)),
                    0xD1 => yield_complete!(indirect_indexed::y_read(&cmp, self, mem_map)),
                    0xD4 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0xD5 => yield_complete!(zero_page_indexed::x_read(&cmp, self, mem_map)),
                    0xD6 => yield_complete!(zero_page_indexed::x_modify(&dec, self, mem_map)),
                    0xD8 => yield_complete!(implicit::run(&cld, self, mem_map)),
                    0xD9 => yield_complete!(absolute_indexed::y_read(&cmp, self, mem_map)),
                    0xDA => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0xDC => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0xDD => yield_complete!(absolute_indexed::x_read(&cmp, self, mem_map)),
                    0xDE => yield_complete!(absolute_indexed::x_modify(&dec, self, mem_map)),
                    0xE0 => yield_complete!(immediate::read(&cpx, self, mem_map)),
                    0xE1 => yield_complete!(indirect_indexed::x_read(&sbc, self, mem_map)),
                    0xE2 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0xE4 => yield_complete!(zero_page::read(&cpx, self, mem_map)),
                    0xE5 => yield_complete!(zero_page::read(&sbc, self, mem_map)),
                    0xE6 => yield_complete!(zero_page::modify(&inc, self, mem_map)),
                    0xE8 => yield_complete!(implicit::run(&inx, self, mem_map)),
                    0xE9 => yield_complete!(immediate::read(&sbc, self, mem_map)),
                    0xEA => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0xEB => yield_complete!(immediate::read(&sbc, self, mem_map)),
                    0xEC => yield_complete!(absolute::read(&cpx, self, mem_map)),
                    0xED => yield_complete!(absolute::read(&sbc, self, mem_map)),
                    0xEE => yield_complete!(absolute::modify(&inc, self, mem_map)),
                    0xF0 => yield_complete!(relative::branch(&beq, self, mem_map)),
                    0xF1 => yield_complete!(indirect_indexed::y_read(&sbc, self, mem_map)),
                    0xF4 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0xF5 => yield_complete!(zero_page_indexed::x_read(&sbc, self, mem_map)),
                    0xF6 => yield_complete!(zero_page_indexed::x_modify(&inc, self, mem_map)),
                    0xF8 => yield_complete!(implicit::run(&sed, self, mem_map)),
                    0xF9 => yield_complete!(absolute_indexed::y_read(&sbc, self, mem_map)),
                    0xFA => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0xFC => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0xFD => yield_complete!(absolute_indexed::x_read(&sbc, self, mem_map)),
                    0xFE => yield_complete!(absolute_indexed::x_modify(&inc, self, mem_map)),
                    _ => { println!("{:?}", instr); unimplemented!() }
                }

                yield CpuCycle::Done { pc: this_pc, op: instr.0, mode: instr.1 }
            }
        }
    }

    fn stack_addr(&self) -> u16 {
        let sp = self.sp.get() as u16;
        0x0100 | sp
    }
    fn stack_inc(&self) {
        self.sp.set(self.sp.get().wrapping_add(1));
    }
    fn stack_dec(&self) {
        self.sp.set(self.sp.get().wrapping_sub(1));
    }
    fn push_stack(&self, mem_map: &Box<&dyn AddressSpace>, v: u8) {
        let addr = self.stack_addr();
        mem_map.write_u8(addr, v);
        self.stack_dec();
    }
    fn pop_stack(&self, mem_map: &Box<&dyn AddressSpace>) -> u8 {
        let v = self.read_stack(mem_map);
        self.stack_inc();
        v
    }
    fn read_stack(&self, mem_map: &Box<&dyn AddressSpace>) -> u8 {
        let addr = self.stack_addr();
        mem_map.read_u8(addr)
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

}

// Operations
impl Cpu {

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

trait BranchOperation {
    fn branch(&self, cpu: &Cpu) -> bool;
}

struct bcc;
impl BranchOperation for bcc {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.flag(Flags::C)
    }
}

struct bcs;
impl BranchOperation for bcs {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.flag(Flags::C)
    }
}

struct beq;
impl BranchOperation for beq {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.flag(Flags::Z)
    }
}

struct bmi;
impl BranchOperation for bmi {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.flag(Flags::N)
    }
}

struct bne;
impl BranchOperation for bne {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.flag(Flags::Z)
    }
}

struct bpl;
impl BranchOperation for bpl {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.flag(Flags::N)
    }
}

struct bvc;
impl BranchOperation for bvc {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.flag(Flags::V)
    }
}

struct bvs;
impl BranchOperation for bvs {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.flag(Flags::V)
    }
}

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

struct and;
impl ReadOperation for and {
    fn operate(&self, cpu: &Cpu, v: u8) {
        cpu.acc.set(cpu.acc.get() & v);
        cpu.set_flags_from_acc();
    }
}

struct bit;
impl ReadOperation for bit {

    fn operate(&self, cpu: &Cpu, v: u8) {
        let r = cpu.acc.get() & v;
        cpu.set_flag(Flags::Z, r == 0);
        cpu.set_flag(Flags::V, (v & 0x40) != 0); // set to the 6th bit of the value
        cpu.set_flag(Flags::N, (v & 0x80) != 0); // set to the 7th bit of the value
    }

}

struct eor;
impl ReadOperation for eor {
    fn operate(&self, cpu: &Cpu, v: u8) {
        cpu.acc.set(cpu.acc.get() ^ v);
        cpu.set_flags_from_acc();
    }
}

struct ora;
impl ReadOperation for ora {
    fn operate(&self, cpu: &Cpu, v: u8) {
        cpu.acc.set(cpu.acc.get() | v);
        cpu.set_flags_from_acc();
    }
}

struct sbc;
impl ReadOperation for sbc {
    fn operate(&self, cpu: &Cpu, v: u8) {
        adc.operate(cpu, !v);
    }
}

fn compare(cpu: &Cpu, a: u8, b: u8) {
    let result = a.wrapping_sub(b);
    cpu.set_flag(Flags::C, a >= b);
    cpu.set_flag(Flags::Z, a == b);
    cpu.set_flag(Flags::N, (result & 0x80) != 0);
}

struct cmp;
impl ReadOperation for cmp {
    fn operate(&self, cpu: &Cpu, v: u8) {
        compare(cpu, cpu.acc.get(), v);
    }
}

struct cpx;
impl ReadOperation for cpx {
    fn operate(&self, cpu: &Cpu, v: u8) {
        compare(cpu, cpu.x.get(), v);
    }
}

struct cpy;
impl ReadOperation for cpy {
    fn operate(&self, cpu: &Cpu, v: u8) {
        compare(cpu, cpu.y.get(), v);
    }
}

// http://obelisk.me.uk/6502/reference.html#LDA
struct lda;
impl ReadOperation for lda {
    fn operate(&self, cpu: &Cpu, value: u8) {
        cpu.acc.set(value);
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

// http://obelisk.me.uk/6502/reference.html#LDY
struct ldy;
impl ReadOperation for ldy {
    fn operate(&self, cpu: &Cpu, value: u8) {
        cpu.y.set(value);
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

struct dec;
impl ModifyOperation for dec {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = v.wrapping_sub(1);
        cpu.set_flags_from(result);
        result
    }
}

struct inc;
impl ModifyOperation for inc {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = v.wrapping_add(1);
        cpu.set_flags_from(result);
        result
    }
}

// http://obelisk.me.uk/6502/reference.html#LSR
struct lsr;
impl ModifyOperation for lsr {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        cpu.set_flag(Flags::C, (v & 0x01) != 0);
        let result = v >> 1;
        cpu.set_flags_from(result);
        result
    }
}

// http://obelisk.me.uk/6502/reference.html#ROL
struct rol;
impl ModifyOperation for rol {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = (v << 1) | cpu.flag(Flags::C) as u8;
        cpu.set_flag(Flags::C, v & 0x80 != 0);
        cpu.set_flags_from(result);
        result
    }
}

// http://obelisk.me.uk/6502/reference.html#ROR
struct ror;
impl ModifyOperation for ror {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = (v >> 1) | ((cpu.flag(Flags::C) as u8) << 7);
        cpu.set_flag(Flags::C, v & 0x01 != 0);
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

// http://obelisk.me.uk/6502/reference.html#STX
struct stx;
impl WriteOperation for stx {
    fn operate(&self, cpu: &Cpu) -> u8 {
        cpu.x.get()
    }
}

// http://obelisk.me.uk/6502/reference.html#STY
struct sty;
impl WriteOperation for sty {
    fn operate(&self, cpu: &Cpu) -> u8 {
        cpu.y.get()
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

struct dex;
impl ImplicitOperation for dex {
    fn operate(&self, cpu: &Cpu) {
        cpu.x.set(dec.operate(cpu, cpu.x.get()));
    }
}

struct dey;
impl ImplicitOperation for dey {
    fn operate(&self, cpu: &Cpu) {
        cpu.y.set(dec.operate(cpu, cpu.y.get()));
    }
}

struct inx;
impl ImplicitOperation for inx {
    fn operate(&self, cpu: &Cpu) {
        cpu.x.set(inc.operate(cpu, cpu.x.get()));
    }
}

struct iny;
impl ImplicitOperation for iny {
    fn operate(&self, cpu: &Cpu) {
        cpu.y.set(inc.operate(cpu, cpu.y.get()));
    }
}

// http://obelisk.me.uk/6502/reference.html#NOP
struct nop;
impl ImplicitOperation for nop {
    fn operate(&self, _: &Cpu) {}
}
impl ReadOperation for nop {
    fn operate(&self, _: &Cpu, _: u8) {}
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

// http://obelisk.me.uk/6502/reference.html#TAY
struct tay;
impl ImplicitOperation for tay {
    fn operate(&self, cpu: &Cpu) {
        cpu.y.set(cpu.acc.get());
        cpu.set_flags_from(cpu.y.get());
    }
}

// http://obelisk.me.uk/6502/reference.html#TAX
struct tax;
impl ImplicitOperation for tax {
    fn operate(&self, cpu: &Cpu) {
        cpu.x.set(cpu.acc.get());
        cpu.set_flags_from(cpu.x.get());
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

// http://obelisk.me.uk/6502/reference.html#TXA
struct txa;
impl ImplicitOperation for txa {
    fn operate(&self, cpu: &Cpu) {
        cpu.acc.set(cpu.x.get());
        cpu.set_flags_from_acc();
    }
}

// http://obelisk.me.uk/6502/reference.html#TXS
struct txs;
impl ImplicitOperation for txs {
    fn operate(&self, cpu: &Cpu) {
        cpu.sp.set(cpu.x.get());
    }
}

// http://obelisk.me.uk/6502/reference.html#TYA
struct tya;
impl ImplicitOperation for tya {
    fn operate(&self, cpu: &Cpu) {
        cpu.acc.set(cpu.y.get());
        cpu.set_flags_from_acc();
    }
}

mod immediate {
    use super::*;

    //  #  address R/W description
    // --- ------- --- ------------------------------------------
    //  1    PC     R  fetch opcode, increment PC
    //  2    PC     R  fetch value, increment PC
    pub(super) fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        move || {
            let value = cpu.pc_read_u8_next(mem_map);
            operation.operate(cpu, value);
            yield CpuCycle::Tick;
        }
    }

}

mod implicit {
    use super::*;

    //  #  address R/W description
    // --- ------- --- -----------------------------------------------
    //  1    PC     R  fetch opcode, increment PC
    //  2    PC     R  read next instruction byte (and throw it away)
    pub(super) fn run<'a, O: ImplicitOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
        move || {
            let _ = cpu.pc_read_u8(mem_map) as u16;
            operation.operate(cpu);
            yield CpuCycle::Tick;
        }
    }
}

mod accumulator {
    use super::*;

    //  #  address R/W description
    // --- ------- --- -----------------------------------------------
    //  1    PC     R  fetch opcode, increment PC
    //  2    PC     R  read next instruction byte (and throw it away)
    pub(super) fn modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
        move || {
            let _ = cpu.pc_read_u8(mem_map) as u16;
            let result = operation.operate(cpu, cpu.acc.get());
            cpu.acc.set(result);
            yield CpuCycle::Tick;
        }
    }
}

mod relative {
    use super::*;
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
    pub(super) fn branch<'a, O: BranchOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield=CpuCycle, Return=()> + 'a {
        move || {
            let operand = cpu.pc_read_u8_next(mem_map) as i8;
            yield CpuCycle::Tick;

            if operation.branch(cpu) {
                let pc = cpu.pc.get() as i16;
                let addr = pc.wrapping_add(operand as i16) as u16;

                if ((pc as u16) & 0xFF00) != (addr & 0xFF00) {
                    // crossing page boundary incurs an additional cycle
                    yield CpuCycle::Tick;
                }

                cpu.pc.set(addr);
                yield CpuCycle::Tick;
            }

        }
    }
}
