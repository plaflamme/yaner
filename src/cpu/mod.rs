#![allow(non_camel_case_types)]

use bitflags::bitflags;
use crate::memory::AddressSpace;
use std::cell::Cell;
use std::fmt::{Display, Formatter, Error};
use std::ops::Generator;

mod mode;
use mode::*;
mod instr;
mod opcode;

pub mod generator;

use opcode::OPCODES;
use opcode::OpCode;
use instr::*;

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
    pub fn write(&self, mem_map: &dyn AddressSpace) -> String{
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
    OpComplete,
    Halt
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
    fn pc_read_u8_next(&self, mem_map: &dyn AddressSpace) -> u8 {
        let pc = self.next_pc();
        mem_map.read_u8(pc)
    }

    // reads u8 at pc
    fn pc_read_u8(&self, mem_map: &dyn AddressSpace) -> u8 {
        let pc = self.pc.get();
        mem_map.read_u8(pc)
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
    fn push_stack(&self, mem_map: &dyn AddressSpace, v: u8) {
        let addr = self.stack_addr();
        mem_map.write_u8(addr, v);
        self.stack_dec();
    }
    fn pop_stack(&self, mem_map: &dyn AddressSpace) -> u8 {
        let v = self.read_stack(mem_map);
        self.stack_inc();
        v
    }
    fn read_stack(&self, mem_map: &dyn AddressSpace) -> u8 {
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

    pub fn run<'a>(&'a self, mem_map: &'a dyn AddressSpace, start_at: Option<u16>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {

        let pc = start_at.unwrap_or_else(|| mem_map.read_u16(0xFFFC));
        self.pc.set(pc);

        move || {
            loop {
                let opcode = self.pc_read_u8_next(mem_map);
                yield CpuCycle::Tick;

                match opcode {
                    0x00 => yield_complete!(stack::brk(self, mem_map)),
                    0x01 => yield_complete!(indirect_indexed::x_read(&ora, self, mem_map)),
                    0x02 => yield CpuCycle::Halt,
                    0x03 => yield_complete!(indirect_indexed::x_modify(&slo, self, mem_map)),
                    0x04 => yield_complete!(zero_page::read(&nop, self, mem_map)),
                    0x05 => yield_complete!(zero_page::read(&ora, self, mem_map)),
                    0x06 => yield_complete!(zero_page::modify(&asl, self, mem_map)),
                    0x07 => yield_complete!(zero_page::modify(&slo, self, mem_map)),
                    0x08 => yield_complete!(stack::php(self, mem_map)),
                    0x09 => yield_complete!(immediate::read(&ora, self, mem_map)),
                    0x0A => yield_complete!(accumulator::modify(&asl, self, mem_map)),
                    0x0B => yield_complete!(immediate::read(&anc, self, mem_map)),
                    0x0C => yield_complete!(absolute::read(&nop, self, mem_map)),
                    0x0D => yield_complete!(absolute::read(&ora, self, mem_map)),
                    0x0E => yield_complete!(absolute::modify(&asl, self, mem_map)),
                    0x0F => yield_complete!(absolute::modify(&slo, self, mem_map)),
                    0x10 => yield_complete!(relative::branch(&bpl, self, mem_map)),
                    0x11 => yield_complete!(indirect_indexed::y_read(&ora, self, mem_map)),
                    0x12 => yield CpuCycle::Halt,
                    0x13 => yield_complete!(indirect_indexed::y_modify(&slo, self, mem_map)),
                    0x14 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0x15 => yield_complete!(zero_page_indexed::x_read(&ora, self, mem_map)),
                    0x16 => yield_complete!(zero_page_indexed::x_modify(&asl, self, mem_map)),
                    0x17 => yield_complete!(zero_page_indexed::x_modify(&slo, self, mem_map)),
                    0x18 => yield_complete!(implicit::run(&clc, self, mem_map)),
                    0x19 => yield_complete!(absolute_indexed::y_read(&ora, self, mem_map)),
                    0x1A => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0x1B => yield_complete!(absolute_indexed::y_modify(&slo, self, mem_map)),
                    0x1C => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0x1D => yield_complete!(absolute_indexed::x_read(&ora, self, mem_map)),
                    0x1E => yield_complete!(absolute_indexed::x_modify(&asl, self, mem_map)),
                    0x1F => yield_complete!(absolute_indexed::x_modify(&slo, self, mem_map)),
                    0x20 => yield_complete!(stack::jsr(self, mem_map)),
                    0x21 => yield_complete!(indirect_indexed::x_read(&and, self, mem_map)),
                    0x22 => yield CpuCycle::Halt,
                    0x23 => yield_complete!(indirect_indexed::x_modify(&rla, self, mem_map)),
                    0x24 => yield_complete!(zero_page::read(&bit, self, mem_map)),
                    0x25 => yield_complete!(zero_page::read(&and, self, mem_map)),
                    0x26 => yield_complete!(zero_page::modify(&rol, self, mem_map)),
                    0x27 => yield_complete!(zero_page::modify(&rla, self, mem_map)),
                    0x28 => yield_complete!(stack::plp(self, mem_map)),
                    0x29 => yield_complete!(immediate::read(&and, self, mem_map)),
                    0x2A => yield_complete!(accumulator::modify(&rol, self, mem_map)),
                    0x2B => yield_complete!(immediate::read(&anc, self, mem_map)),
                    0x2C => yield_complete!(absolute::read(&bit, self, mem_map)),
                    0x2D => yield_complete!(absolute::read(&and, self, mem_map)),
                    0x2E => yield_complete!(absolute::modify(&rol, self, mem_map)),
                    0x2F => yield_complete!(absolute::modify(&rla, self, mem_map)),
                    0x30 => yield_complete!(relative::branch(&bmi, self, mem_map)),
                    0x31 => yield_complete!(indirect_indexed::y_read(&and, self, mem_map)),
                    0x32 => yield CpuCycle::Halt,
                    0x33 => yield_complete!(indirect_indexed::y_modify(&rla, self, mem_map)),
                    0x34 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0x35 => yield_complete!(zero_page_indexed::x_read(&and, self, mem_map)),
                    0x36 => yield_complete!(zero_page_indexed::x_modify(&rol, self, mem_map)),
                    0x37 => yield_complete!(zero_page_indexed::x_modify(&rla, self, mem_map)),
                    0x38 => yield_complete!(implicit::run(&sec, self, mem_map)),
                    0x39 => yield_complete!(absolute_indexed::y_read(&and, self, mem_map)),
                    0x3A => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0x3B => yield_complete!(absolute_indexed::y_modify(&rla, self, mem_map)),
                    0x3C => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0x3D => yield_complete!(absolute_indexed::x_read(&and, self, mem_map)),
                    0x3E => yield_complete!(absolute_indexed::x_modify(&rol, self, mem_map)),
                    0x3F => yield_complete!(absolute_indexed::x_modify(&rla, self, mem_map)),
                    0x40 => yield_complete!(stack::rti(self, mem_map)),
                    0x41 => yield_complete!(indirect_indexed::x_read(&eor, self, mem_map)),
                    0x42 => yield CpuCycle::Halt,
                    0x43 => yield_complete!(indirect_indexed::x_modify(&sre, self, mem_map)),
                    0x44 => yield_complete!(zero_page::read(&nop, self, mem_map)),
                    0x45 => yield_complete!(zero_page::read(&eor, self, mem_map)),
                    0x46 => yield_complete!(zero_page::modify(&lsr, self, mem_map)),
                    0x47 => yield_complete!(zero_page::modify(&sre, self, mem_map)),
                    0x48 => yield_complete!(stack::pha(self, mem_map)),
                    0x49 => yield_complete!(immediate::read(&eor, self, mem_map)),
                    0x4A => yield_complete!(accumulator::modify(&lsr, self, mem_map)),
                    0x4B => yield_complete!(immediate::read(&alr, self, mem_map)),
                    0x4C => yield_complete!(absolute::jmp(self, mem_map)),
                    0x4D => yield_complete!(absolute::read(&eor, self, mem_map)),
                    0x4E => yield_complete!(absolute::modify(&lsr, self, mem_map)),
                    0x4F => yield_complete!(absolute::modify(&sre, self, mem_map)),
                    0x50 => yield_complete!(relative::branch(&bvc, self, mem_map)),
                    0x51 => yield_complete!(indirect_indexed::y_read(&eor, self, mem_map)),
                    0x52 => yield CpuCycle::Halt,
                    0x53 => yield_complete!(indirect_indexed::y_modify(&sre, self, mem_map)),
                    0x54 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0x55 => yield_complete!(zero_page_indexed::x_read(&eor, self, mem_map)),
                    0x56 => yield_complete!(zero_page_indexed::x_modify(&lsr, self, mem_map)),
                    0x57 => yield_complete!(zero_page_indexed::x_modify(&sre, self, mem_map)),
                    0x58 => yield_complete!(implicit::run(&cli, self, mem_map)),
                    0x59 => yield_complete!(absolute_indexed::y_read(&eor, self, mem_map)),
                    0x5A => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0x5B => yield_complete!(absolute_indexed::y_modify(&sre, self, mem_map)),
                    0x5C => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0x5D => yield_complete!(absolute_indexed::x_read(&eor, self, mem_map)),
                    0x5E => yield_complete!(absolute_indexed::x_modify(&lsr, self, mem_map)),
                    0x5F => yield_complete!(absolute_indexed::x_modify(&sre, self, mem_map)),
                    0x60 => yield_complete!(stack::rts(self, mem_map)),
                    0x61 => yield_complete!(indirect_indexed::x_read(&adc, self, mem_map)),
                    0x62 => yield CpuCycle::Halt,
                    0x63 => yield_complete!(indirect_indexed::x_modify(&rra, self, mem_map)),
                    0x64 => yield_complete!(zero_page::read(&nop, self, mem_map)),
                    0x65 => yield_complete!(zero_page::read(&adc, self, mem_map)),
                    0x66 => yield_complete!(zero_page::modify(&ror, self, mem_map)),
                    0x67 => yield_complete!(zero_page::modify(&rra, self, mem_map)),
                    0x68 => yield_complete!(stack::pla(self, mem_map)),
                    0x69 => yield_complete!(immediate::read(&adc, self, mem_map)),
                    0x6A => yield_complete!(accumulator::modify(&ror, self, mem_map)),
                    0x6B => yield_complete!(immediate::read(&arr, self, mem_map)),
                    0x6C => yield_complete!(indirect::jmp(self, mem_map)),
                    0x6D => yield_complete!(absolute::read(&adc, self, mem_map)),
                    0x6E => yield_complete!(absolute::modify(&ror, self, mem_map)),
                    0x6F => yield_complete!(absolute::modify(&rra, self, mem_map)),
                    0x70 => yield_complete!(relative::branch(&bvs, self, mem_map)),
                    0x71 => yield_complete!(indirect_indexed::y_read(&adc, self, mem_map)),
                    0x72 => yield CpuCycle::Halt,
                    0x73 => yield_complete!(indirect_indexed::y_modify(&rra, self, mem_map)),
                    0x74 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0x75 => yield_complete!(zero_page_indexed::x_read(&adc, self, mem_map)),
                    0x76 => yield_complete!(zero_page_indexed::x_modify(&ror, self, mem_map)),
                    0x77 => yield_complete!(zero_page_indexed::x_modify(&rra, self, mem_map)),
                    0x78 => yield_complete!(implicit::run(&sei, self, mem_map)),
                    0x79 => yield_complete!(absolute_indexed::y_read(&adc, self, mem_map)),
                    0x7A => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0x7B => yield_complete!(absolute_indexed::y_modify(&rra, self, mem_map)),
                    0x7C => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0x7D => yield_complete!(absolute_indexed::x_read(&adc, self, mem_map)),
                    0x7E => yield_complete!(absolute_indexed::x_modify(&ror, self, mem_map)),
                    0x7F => yield_complete!(absolute_indexed::x_modify(&rra, self, mem_map)),
                    0x80 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0x81 => yield_complete!(indirect_indexed::x_write(&sta, self, mem_map)),
                    0x82 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0x83 => yield_complete!(indirect_indexed::x_write(&sax, self, mem_map)),
                    0x84 => yield_complete!(zero_page::write(&sty, self, mem_map)),
                    0x85 => yield_complete!(zero_page::write(&sta, self, mem_map)),
                    0x86 => yield_complete!(zero_page::write(&stx, self, mem_map)),
                    0x87 => yield_complete!(zero_page::write(&sax, self, mem_map)),
                    0x88 => yield_complete!(implicit::run(&dey, self, mem_map)),
                    0x89 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0x8A => yield_complete!(implicit::run(&txa, self, mem_map)),
                    0x8C => yield_complete!(absolute::write(&sty, self, mem_map)),
                    0x8D => yield_complete!(absolute::write(&sta, self, mem_map)),
                    0x8E => yield_complete!(absolute::write(&stx, self, mem_map)),
                    0x8F => yield_complete!(absolute::write(&sax, self, mem_map)),
                    0x90 => yield_complete!(relative::branch(&bcc, self, mem_map)),
                    0x91 => yield_complete!(indirect_indexed::y_write(&sta, self, mem_map)),
                    0x92 => yield CpuCycle::Halt,
                    0x93 => yield_complete!(indirect_indexed::y_modify(&ahx, self, mem_map)),
                    0x94 => yield_complete!(zero_page_indexed::x_write(&sty, self, mem_map)),
                    0x95 => yield_complete!(zero_page_indexed::x_write(&sta, self, mem_map)),
                    0x96 => yield_complete!(zero_page_indexed::y_write(&stx, self, mem_map)),
                    0x97 => yield_complete!(zero_page_indexed::y_write(&sax, self, mem_map)),
                    0x98 => yield_complete!(implicit::run(&tya, self, mem_map)),
                    0x99 => yield_complete!(absolute_indexed::y_write(&sta, self, mem_map)),
                    0x9A => yield_complete!(implicit::run(&txs, self, mem_map)),
                    0x9B => yield_complete!(absolute_indexed::y_modify(&tas, self, mem_map)),
                    0x9C => yield_complete!(absolute_indexed::x_modify(&shy, self, mem_map)),
                    0x9D => yield_complete!(absolute_indexed::x_write(&sta, self, mem_map)),
                    0x9E => yield_complete!(absolute_indexed::y_modify(&shx, self, mem_map)),
                    0x9F => yield_complete!(absolute_indexed::y_modify(&ahx, self, mem_map)),
                    0xA0 => yield_complete!(immediate::read(&ldy, self, mem_map)),
                    0xA1 => yield_complete!(indirect_indexed::x_read(&lda, self, mem_map)),
                    0xA2 => yield_complete!(immediate::read(&ldx, self, mem_map)),
                    0xA3 => yield_complete!(indirect_indexed::x_read(&lax, self, mem_map)),
                    0xA4 => yield_complete!(zero_page::read(&ldy, self, mem_map)),
                    0xA5 => yield_complete!(zero_page::read(&lda, self, mem_map)),
                    0xA6 => yield_complete!(zero_page::read(&ldx, self, mem_map)),
                    0xA7 => yield_complete!(zero_page::read(&lax, self, mem_map)),
                    0xA8 => yield_complete!(implicit::run(&tay, self, mem_map)),
                    0xA9 => yield_complete!(immediate::read(&lda, self, mem_map)),
                    0xAA => yield_complete!(implicit::run(&tax, self, mem_map)),
                    0xAB => yield_complete!(immediate::read(&lax, self, mem_map)),
                    0xAC => yield_complete!(absolute::read(&ldy, self, mem_map)),
                    0xAD => yield_complete!(absolute::read(&lda, self, mem_map)),
                    0xAE => yield_complete!(absolute::read(&ldx, self, mem_map)),
                    0xAF => yield_complete!(absolute::read(&lax, self, mem_map)),
                    0xB0 => yield_complete!(relative::branch(&bcs, self, mem_map)),
                    0xB1 => yield_complete!(indirect_indexed::y_read(&lda, self, mem_map)),
                    0xB2 => yield CpuCycle::Halt,
                    0xB3 => yield_complete!(indirect_indexed::y_read(&lax, self, mem_map)),
                    0xB4 => yield_complete!(zero_page_indexed::x_read(&ldy, self, mem_map)),
                    0xB5 => yield_complete!(zero_page_indexed::x_read(&lda, self, mem_map)),
                    0xB6 => yield_complete!(zero_page_indexed::y_read(&ldx, self, mem_map)),
                    0xB7 => yield_complete!(zero_page_indexed::y_read(&lax, self, mem_map)),
                    0xB8 => yield_complete!(implicit::run(&clv, self, mem_map)),
                    0xB9 => yield_complete!(absolute_indexed::y_read(&lda, self, mem_map)),
                    0xBA => yield_complete!(implicit::run(&tsx, self, mem_map)),
                    0xBB => yield_complete!(absolute_indexed::y_read(&las, self, mem_map)),
                    0xBC => yield_complete!(absolute_indexed::x_read(&ldy, self, mem_map)),
                    0xBD => yield_complete!(absolute_indexed::x_read(&lda, self, mem_map)),
                    0xBE => yield_complete!(absolute_indexed::y_read(&ldx, self, mem_map)),
                    0xBF => yield_complete!(absolute_indexed::y_read(&lax, self, mem_map)),
                    0xC0 => yield_complete!(immediate::read(&cpy, self, mem_map)),
                    0xC1 => yield_complete!(indirect_indexed::x_read(&cmp, self, mem_map)),
                    0xC2 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0xC3 => yield_complete!(indirect_indexed::x_modify(&dcp, self, mem_map)),
                    0xC4 => yield_complete!(zero_page::read(&cpy, self, mem_map)),
                    0xC5 => yield_complete!(zero_page::read(&cmp, self, mem_map)),
                    0xC6 => yield_complete!(zero_page::modify(&dec, self, mem_map)),
                    0xC7 => yield_complete!(zero_page::modify(&dcp, self, mem_map)),
                    0xC8 => yield_complete!(implicit::run(&iny, self, mem_map)),
                    0xC9 => yield_complete!(immediate::read(&cmp, self, mem_map)),
                    0xCA => yield_complete!(implicit::run(&dex, self, mem_map)),
                    0xCB => yield_complete!(immediate::read(&axs, self, mem_map)),
                    0xCC => yield_complete!(absolute::read(&cpy, self, mem_map)),
                    0xCD => yield_complete!(absolute::read(&cmp, self, mem_map)),
                    0xCE => yield_complete!(absolute::modify(&dec, self, mem_map)),
                    0xCF => yield_complete!(absolute::modify(&dcp, self, mem_map)),
                    0xD0 => yield_complete!(relative::branch(&bne, self, mem_map)),
                    0xD1 => yield_complete!(indirect_indexed::y_read(&cmp, self, mem_map)),
                    0xD2 => yield CpuCycle::Halt,
                    0xD3 => yield_complete!(indirect_indexed::y_modify(&dcp, self, mem_map)),
                    0xD4 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0xD5 => yield_complete!(zero_page_indexed::x_read(&cmp, self, mem_map)),
                    0xD6 => yield_complete!(zero_page_indexed::x_modify(&dec, self, mem_map)),
                    0xD7 => yield_complete!(zero_page_indexed::x_modify(&dcp, self, mem_map)),
                    0xD8 => yield_complete!(implicit::run(&cld, self, mem_map)),
                    0xD9 => yield_complete!(absolute_indexed::y_read(&cmp, self, mem_map)),
                    0xDA => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0xDB => yield_complete!(absolute_indexed::y_modify(&dcp, self, mem_map)),
                    0xDC => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0xDD => yield_complete!(absolute_indexed::x_read(&cmp, self, mem_map)),
                    0xDE => yield_complete!(absolute_indexed::x_modify(&dec, self, mem_map)),
                    0xDF => yield_complete!(absolute_indexed::x_modify(&dcp, self, mem_map)),
                    0xE0 => yield_complete!(immediate::read(&cpx, self, mem_map)),
                    0xE1 => yield_complete!(indirect_indexed::x_read(&sbc, self, mem_map)),
                    0xE2 => yield_complete!(immediate::read(&nop, self, mem_map)),
                    0xE3 => yield_complete!(indirect_indexed::x_modify(&isc, self, mem_map)),
                    0xE4 => yield_complete!(zero_page::read(&cpx, self, mem_map)),
                    0xE5 => yield_complete!(zero_page::read(&sbc, self, mem_map)),
                    0xE6 => yield_complete!(zero_page::modify(&inc, self, mem_map)),
                    0xE7 => yield_complete!(zero_page::modify(&isc, self, mem_map)),
                    0xE8 => yield_complete!(implicit::run(&inx, self, mem_map)),
                    0xE9 => yield_complete!(immediate::read(&sbc, self, mem_map)),
                    0xEA => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0xEB => yield_complete!(immediate::read(&sbc, self, mem_map)),
                    0xEC => yield_complete!(absolute::read(&cpx, self, mem_map)),
                    0xED => yield_complete!(absolute::read(&sbc, self, mem_map)),
                    0xEE => yield_complete!(absolute::modify(&inc, self, mem_map)),
                    0xEF => yield_complete!(absolute::modify(&isc, self, mem_map)),
                    0xF0 => yield_complete!(relative::branch(&beq, self, mem_map)),
                    0xF1 => yield_complete!(indirect_indexed::y_read(&sbc, self, mem_map)),
                    0xF2 => yield CpuCycle::Halt,
                    0xF3 => yield_complete!(indirect_indexed::y_modify(&isc, self, mem_map)),
                    0xF4 => yield_complete!(zero_page_indexed::x_read(&nop, self, mem_map)),
                    0xF5 => yield_complete!(zero_page_indexed::x_read(&sbc, self, mem_map)),
                    0xF6 => yield_complete!(zero_page_indexed::x_modify(&inc, self, mem_map)),
                    0xF7 => yield_complete!(zero_page_indexed::x_modify(&isc, self, mem_map)),
                    0xF8 => yield_complete!(implicit::run(&sed, self, mem_map)),
                    0xF9 => yield_complete!(absolute_indexed::y_read(&sbc, self, mem_map)),
                    0xFA => yield_complete!(implicit::run(&nop, self, mem_map)),
                    0xFB => yield_complete!(absolute_indexed::y_modify(&isc, self, mem_map)),
                    0xFC => yield_complete!(absolute_indexed::x_read(&nop, self, mem_map)),
                    0xFD => yield_complete!(absolute_indexed::x_read(&sbc, self, mem_map)),
                    0xFE => yield_complete!(absolute_indexed::x_modify(&inc, self, mem_map)),
                    0xFF => yield_complete!(absolute_indexed::x_modify(&isc, self, mem_map)),
                    _ => {
                        println!("{:?}", &OPCODES[opcode as usize]); unimplemented!()
                    }
                }

                yield CpuCycle::OpComplete
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
    // this includes addr because AHX, SHX and SHY operate on the high byte of the target address
    //   instead of the value
    fn modify(&self, cpu: &Cpu, _addr: u16, value: u8) -> u8 {
        self.operate(cpu, value)
    }

    fn operate(&self, cpu: &Cpu, value: u8) -> u8;
}

trait ReadOperation {
    fn operate(&self, cpu: &Cpu, value: u8);
}

trait WriteOperation {
    fn operate(&self, cpu: &Cpu) -> u8;
}

pub struct CpuAddressSpace<'a> {
    ram: &'a dyn AddressSpace,
    ppu: &'a dyn AddressSpace,
    mapper: &'a dyn AddressSpace,
    oam_dma: Cell<Option<u8>>
}

impl<'a> CpuAddressSpace<'a> {
    pub fn new(ram: &'a dyn AddressSpace, ppu: &'a dyn AddressSpace, mapper: &'a dyn AddressSpace) -> Self {
        CpuAddressSpace { ram, ppu, mapper, oam_dma: Cell::new(None) }
    }

    // This will return last write to OAM DMA and then None until the next write
    pub fn dma_latch(&self) -> Option<u8> {
        let v = self.oam_dma.get();
        if v.is_some() { self.oam_dma.set(None) };
        v
    }
}

impl<'a> crate::memory::AddressSpace for CpuAddressSpace<'a> {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2000..=0x2007 => self.ppu.read_u8(addr), // PPU
            0x2008..=0x3FFF => self.ppu.read_u8(0x2000 + (addr % 8)), // PPU mirror

            0x4000..=0x4017 => unimplemented!(), // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.mapper.read_u8(addr), // PRG ROM/RAM and mapper
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x8000, value),

            0x2000..=0x2007 => self.ppu.write_u8(addr, value), // PPU
            0x2008..=0x3FFF => self.ppu.write_u8(0x2000 + (addr % 8), value), // PPU mirror

            0x4014 => self.oam_dma.set(Some(value)),

            0x4000..=0x4017 => (), // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.mapper.write_u8(addr, value), // PRG ROM/RAM and mapper
        }
    }
}
