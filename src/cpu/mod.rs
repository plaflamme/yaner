#![allow(non_camel_case_types)]

use crate::memory::{AddressSpace, Ram2KB};
use bitflags::bitflags;
use std::cell::{Cell, RefCell};
use std::fmt::{Display, Error, Formatter};
use std::ops::Generator;

mod mode;
use mode::*;
pub mod debug;
mod instr;
pub mod opcode;

pub mod generator;

use crate::cartridge::Mapper;
use crate::ppu::MemoryMappedRegisters;
use instr::*;
use opcode::OpCode;
use opcode::OPCODES;
use std::rc::Rc;

// http://obelisk.me.uk/6502/reference.html
#[derive(Debug, Clone, Copy, PartialEq)]
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

pub struct Operand(pub String, pub u8);

#[derive(Debug, Clone, Copy)]
pub enum AddressingMode {
    Immediate, // imm

    ZeroPage,  // zp $00
    ZeroPageX, // zpx $00,X
    ZeroPageY, // zpy $00,Y

    Indirect,  // ind
    IndirectX, // izx ($00,X)
    IndirectY, // izy ($00),Y

    Absolute,  // abs $0000
    AbsoluteX, // abs $0000,X
    AbsoluteY, // abs $0000,Y

    Accumulator, // A
    Relative,    // rel

    Implicit,
}

impl AddressingMode {
    pub fn operand(&self, pc: u16, addr_space: &dyn AddressSpace) -> Operand {
        let addr = pc.wrapping_add(1); // skip opcode
        match self {
            AddressingMode::Immediate => Operand(format!("#${:02X}", addr_space.read_u8(addr)), 1),

            AddressingMode::ZeroPage => Operand(format!("${:02X}", addr_space.read_u8(addr)), 1),
            AddressingMode::ZeroPageX => Operand(format!("${:02X},X", addr_space.read_u8(addr)), 1),
            AddressingMode::ZeroPageY => Operand(format!("${:02X},Y", addr_space.read_u8(addr)), 1),

            AddressingMode::Indirect => Operand(format!("(${:02X})", addr_space.read_u8(addr)), 1),
            AddressingMode::IndirectX => {
                Operand(format!("(${:02X},X)", addr_space.read_u8(addr)), 1)
            }
            AddressingMode::IndirectY => {
                Operand(format!("(${:02X}),Y", addr_space.read_u8(addr)), 1)
            }

            AddressingMode::Absolute => Operand(format!("${:04X}", addr_space.read_u16(addr)), 2),
            AddressingMode::AbsoluteX => {
                Operand(format!("${:04X},X", addr_space.read_u16(addr)), 2)
            }
            AddressingMode::AbsoluteY => {
                Operand(format!("${:04X},Y", addr_space.read_u16(addr)), 2)
            }

            AddressingMode::Accumulator => Operand("A".to_owned(), 0),

            AddressingMode::Relative => Operand(format!("+${:02X}", addr_space.read_u8(addr)), 1),

            AddressingMode::Implicit => Operand("".to_owned(), 0),
        }
    }
}

// http://wiki.nesdev.com/w/index.php/Status_flags
bitflags!(
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

#[derive(Debug, Clone, Copy)]
pub enum Interrupt {
    Nmi,
    Brk,
}

// http://nesdev.com/6502_cpu.txt
// http://wiki.nesdev.com/w/index.php/CPU_ALL
pub struct Cpu {
    acc: Cell<u8>,
    x: Cell<u8>,
    y: Cell<u8>,
    flags: Cell<Flags>,
    sp: Cell<u8>,
    pc: Cell<u16>,

    pub bus: CpuBus,
}

impl Display for Cpu {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let pc = self.pc.get();
        let opcode = self.bus.read_u8(pc);
        let OpCode(op, mode) = &OPCODES[opcode as usize];
        let Operand(operand, _) = mode.operand(self.pc.get(), &self.bus);
        let flags = format!("({:?})", self.flags.get());
        write!(
            f,
            "{:04X} {:?} {:<7} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} {:<27} SP:{:02X?}",
            pc,
            op,
            operand,
            self.acc.get(),
            self.x.get(),
            self.y.get(),
            self.flags.get().bits(),
            flags,
            self.sp.get()
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OpTrace {
    // the resulting address used by the instruction
    Addr(u16),

    // the resulting address used by the instruction,
    //   as well as the intermediate, partially computed address and whether it resulted
    //   in an extra cycle
    AddrIndexed { addr: u16, unfixed: u16, oops: bool },

    // No relevant information
    Implicit,
}

#[derive(Debug)]
pub enum CpuCycle {
    Tick,
    OpComplete(OpCode, OpTrace),
    Halt,
}

impl Cpu {
    pub fn new(bus: CpuBus) -> Self {
        // http://wiki.nesdev.com/w/index.php/CPU_ALL#Power_up_state
        Cpu {
            acc: Cell::new(0),
            x: Cell::new(0),
            y: Cell::new(0),
            flags: Cell::new(Flags::from_bits_truncate(0x24)), // The wiki says 0x34, but nestest seems to use 0x24
            sp: Cell::new(0xFD),
            pc: Cell::new(0),

            bus,
        }
    }

    // reads pc and advances by one
    fn next_pc(&self) -> u16 {
        let pc = self.pc.get();
        self.pc.set(pc.wrapping_add(1));
        pc
    }

    // advances pc by one and reads u8
    fn next_pc_read_u8(&self) -> u8 {
        let pc = self.next_pc();
        self.bus.read_u8(pc)
    }

    // reads u8 at pc
    fn pc_read_u8(&self) -> u8 {
        let pc = self.pc.get();
        self.bus.read_u8(pc)
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
    fn push_stack(&self, v: u8) {
        let addr = self.stack_addr();
        self.bus.write_u8(addr, v);
        self.stack_dec();
    }
    fn pop_stack(&self) -> u8 {
        let v = self.read_stack();
        self.stack_inc();
        v
    }
    fn read_stack(&self) -> u8 {
        let addr = self.stack_addr();
        self.bus.read_u8(addr)
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

    pub fn run<'a>(
        &'a self,
        start_at: Option<u16>,
    ) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
        let pc = start_at.unwrap_or_else(|| self.bus.read_u16(0xFFFC));
        self.pc.set(pc);

        // used to delay interrupts by one op
        // TODO: this probably requires more granular timing
        let mut interrupt: Option<Interrupt> = None;

        move || loop {
            if let Some(intr) = interrupt {
                let trace = yield_complete!(stack::interrupt(self, intr));
                yield CpuCycle::OpComplete(OPCODES[0x00].clone(), trace);
            }

            let opcode = self.next_pc_read_u8();
            yield CpuCycle::Tick;

            let trace = match opcode {
                0x00 => yield_complete!(stack::brk(self)),
                0x01 => yield_complete!(indirect_indexed::x_read(&ora, self)),
                0x02 => OpTrace::Implicit,
                0x03 => yield_complete!(indirect_indexed::x_modify(&slo, self)),
                0x04 => yield_complete!(zero_page::read(&nop, self)),
                0x05 => yield_complete!(zero_page::read(&ora, self)),
                0x06 => yield_complete!(zero_page::modify(&asl, self)),
                0x07 => yield_complete!(zero_page::modify(&slo, self)),
                0x08 => yield_complete!(stack::php(self)),
                0x09 => immediate::read(&ora, self),
                0x0A => accumulator::modify(&asl, self),
                0x0B => immediate::read(&anc, self),
                0x0C => yield_complete!(absolute::read(&nop, self)),
                0x0D => yield_complete!(absolute::read(&ora, self)),
                0x0E => yield_complete!(absolute::modify(&asl, self)),
                0x0F => yield_complete!(absolute::modify(&slo, self)),
                0x10 => yield_complete!(relative::branch(&bpl, self)),
                0x11 => yield_complete!(indirect_indexed::y_read(&ora, self)),
                0x12 => OpTrace::Implicit,
                0x13 => yield_complete!(indirect_indexed::y_modify(&slo, self)),
                0x14 => yield_complete!(zero_page_indexed::x_read(&nop, self)),
                0x15 => yield_complete!(zero_page_indexed::x_read(&ora, self)),
                0x16 => yield_complete!(zero_page_indexed::x_modify(&asl, self)),
                0x17 => yield_complete!(zero_page_indexed::x_modify(&slo, self)),
                0x18 => implicit::run(&clc, self),
                0x19 => yield_complete!(absolute_indexed::y_read(&ora, self)),
                0x1A => implicit::run(&nop, self),
                0x1B => yield_complete!(absolute_indexed::y_modify(&slo, self)),
                0x1C => yield_complete!(absolute_indexed::x_read(&nop, self)),
                0x1D => yield_complete!(absolute_indexed::x_read(&ora, self)),
                0x1E => yield_complete!(absolute_indexed::x_modify(&asl, self)),
                0x1F => yield_complete!(absolute_indexed::x_modify(&slo, self)),
                0x20 => yield_complete!(stack::jsr(self)),
                0x21 => yield_complete!(indirect_indexed::x_read(&and, self)),
                0x22 => OpTrace::Implicit,
                0x23 => yield_complete!(indirect_indexed::x_modify(&rla, self)),
                0x24 => yield_complete!(zero_page::read(&bit, self)),
                0x25 => yield_complete!(zero_page::read(&and, self)),
                0x26 => yield_complete!(zero_page::modify(&rol, self)),
                0x27 => yield_complete!(zero_page::modify(&rla, self)),
                0x28 => yield_complete!(stack::plp(self)),
                0x29 => immediate::read(&and, self),
                0x2A => accumulator::modify(&rol, self),
                0x2B => immediate::read(&anc, self),
                0x2C => yield_complete!(absolute::read(&bit, self)),
                0x2D => yield_complete!(absolute::read(&and, self)),
                0x2E => yield_complete!(absolute::modify(&rol, self)),
                0x2F => yield_complete!(absolute::modify(&rla, self)),
                0x30 => yield_complete!(relative::branch(&bmi, self)),
                0x31 => yield_complete!(indirect_indexed::y_read(&and, self)),
                0x32 => OpTrace::Implicit,
                0x33 => yield_complete!(indirect_indexed::y_modify(&rla, self)),
                0x34 => yield_complete!(zero_page_indexed::x_read(&nop, self)),
                0x35 => yield_complete!(zero_page_indexed::x_read(&and, self)),
                0x36 => yield_complete!(zero_page_indexed::x_modify(&rol, self)),
                0x37 => yield_complete!(zero_page_indexed::x_modify(&rla, self)),
                0x38 => implicit::run(&sec, self),
                0x39 => yield_complete!(absolute_indexed::y_read(&and, self)),
                0x3A => implicit::run(&nop, self),
                0x3B => yield_complete!(absolute_indexed::y_modify(&rla, self)),
                0x3C => yield_complete!(absolute_indexed::x_read(&nop, self)),
                0x3D => yield_complete!(absolute_indexed::x_read(&and, self)),
                0x3E => yield_complete!(absolute_indexed::x_modify(&rol, self)),
                0x3F => yield_complete!(absolute_indexed::x_modify(&rla, self)),
                0x40 => yield_complete!(stack::rti(self)),
                0x41 => yield_complete!(indirect_indexed::x_read(&eor, self)),
                0x42 => OpTrace::Implicit,
                0x43 => yield_complete!(indirect_indexed::x_modify(&sre, self)),
                0x44 => yield_complete!(zero_page::read(&nop, self)),
                0x45 => yield_complete!(zero_page::read(&eor, self)),
                0x46 => yield_complete!(zero_page::modify(&lsr, self)),
                0x47 => yield_complete!(zero_page::modify(&sre, self)),
                0x48 => yield_complete!(stack::pha(self)),
                0x49 => immediate::read(&eor, self),
                0x4A => accumulator::modify(&lsr, self),
                0x4B => immediate::read(&alr, self),
                0x4C => yield_complete!(absolute::jmp(self)),
                0x4D => yield_complete!(absolute::read(&eor, self)),
                0x4E => yield_complete!(absolute::modify(&lsr, self)),
                0x4F => yield_complete!(absolute::modify(&sre, self)),
                0x50 => yield_complete!(relative::branch(&bvc, self)),
                0x51 => yield_complete!(indirect_indexed::y_read(&eor, self)),
                0x52 => OpTrace::Implicit,
                0x53 => yield_complete!(indirect_indexed::y_modify(&sre, self)),
                0x54 => yield_complete!(zero_page_indexed::x_read(&nop, self)),
                0x55 => yield_complete!(zero_page_indexed::x_read(&eor, self)),
                0x56 => yield_complete!(zero_page_indexed::x_modify(&lsr, self)),
                0x57 => yield_complete!(zero_page_indexed::x_modify(&sre, self)),
                0x58 => implicit::run(&cli, self),
                0x59 => yield_complete!(absolute_indexed::y_read(&eor, self)),
                0x5A => implicit::run(&nop, self),
                0x5B => yield_complete!(absolute_indexed::y_modify(&sre, self)),
                0x5C => yield_complete!(absolute_indexed::x_read(&nop, self)),
                0x5D => yield_complete!(absolute_indexed::x_read(&eor, self)),
                0x5E => yield_complete!(absolute_indexed::x_modify(&lsr, self)),
                0x5F => yield_complete!(absolute_indexed::x_modify(&sre, self)),
                0x60 => yield_complete!(stack::rts(self)),
                0x61 => yield_complete!(indirect_indexed::x_read(&adc, self)),
                0x62 => OpTrace::Implicit,
                0x63 => yield_complete!(indirect_indexed::x_modify(&rra, self)),
                0x64 => yield_complete!(zero_page::read(&nop, self)),
                0x65 => yield_complete!(zero_page::read(&adc, self)),
                0x66 => yield_complete!(zero_page::modify(&ror, self)),
                0x67 => yield_complete!(zero_page::modify(&rra, self)),
                0x68 => yield_complete!(stack::pla(self)),
                0x69 => immediate::read(&adc, self),
                0x6A => accumulator::modify(&ror, self),
                0x6B => immediate::read(&arr, self),
                0x6C => yield_complete!(indirect::jmp(self)),
                0x6D => yield_complete!(absolute::read(&adc, self)),
                0x6E => yield_complete!(absolute::modify(&ror, self)),
                0x6F => yield_complete!(absolute::modify(&rra, self)),
                0x70 => yield_complete!(relative::branch(&bvs, self)),
                0x71 => yield_complete!(indirect_indexed::y_read(&adc, self)),
                0x72 => OpTrace::Implicit,
                0x73 => yield_complete!(indirect_indexed::y_modify(&rra, self)),
                0x74 => yield_complete!(zero_page_indexed::x_read(&nop, self)),
                0x75 => yield_complete!(zero_page_indexed::x_read(&adc, self)),
                0x76 => yield_complete!(zero_page_indexed::x_modify(&ror, self)),
                0x77 => yield_complete!(zero_page_indexed::x_modify(&rra, self)),
                0x78 => implicit::run(&sei, self),
                0x79 => yield_complete!(absolute_indexed::y_read(&adc, self)),
                0x7A => implicit::run(&nop, self),
                0x7B => yield_complete!(absolute_indexed::y_modify(&rra, self)),
                0x7C => yield_complete!(absolute_indexed::x_read(&nop, self)),
                0x7D => yield_complete!(absolute_indexed::x_read(&adc, self)),
                0x7E => yield_complete!(absolute_indexed::x_modify(&ror, self)),
                0x7F => yield_complete!(absolute_indexed::x_modify(&rra, self)),
                0x80 => immediate::read(&nop, self),
                0x81 => yield_complete!(indirect_indexed::x_write(&sta, self)),
                0x82 => immediate::read(&nop, self),
                0x83 => yield_complete!(indirect_indexed::x_write(&sax, self)),
                0x84 => yield_complete!(zero_page::write(&sty, self)),
                0x85 => yield_complete!(zero_page::write(&sta, self)),
                0x86 => yield_complete!(zero_page::write(&stx, self)),
                0x87 => yield_complete!(zero_page::write(&sax, self)),
                0x88 => implicit::run(&dey, self),
                0x89 => immediate::read(&nop, self),
                0x8A => implicit::run(&txa, self),
                0x8C => yield_complete!(absolute::write(&sty, self)),
                0x8D => yield_complete!(absolute::write(&sta, self)),
                0x8E => yield_complete!(absolute::write(&stx, self)),
                0x8F => yield_complete!(absolute::write(&sax, self)),
                0x90 => yield_complete!(relative::branch(&bcc, self)),
                0x91 => yield_complete!(indirect_indexed::y_write(&sta, self)),
                0x92 => OpTrace::Implicit,
                0x93 => yield_complete!(indirect_indexed::y_modify(&ahx, self)),
                0x94 => yield_complete!(zero_page_indexed::x_write(&sty, self)),
                0x95 => yield_complete!(zero_page_indexed::x_write(&sta, self)),
                0x96 => yield_complete!(zero_page_indexed::y_write(&stx, self)),
                0x97 => yield_complete!(zero_page_indexed::y_write(&sax, self)),
                0x98 => implicit::run(&tya, self),
                0x99 => yield_complete!(absolute_indexed::y_write(&sta, self)),
                0x9A => implicit::run(&txs, self),
                0x9B => yield_complete!(absolute_indexed::y_modify(&tas, self)),
                0x9C => yield_complete!(absolute_indexed::x_modify(&shy, self)),
                0x9D => yield_complete!(absolute_indexed::x_write(&sta, self)),
                0x9E => yield_complete!(absolute_indexed::y_modify(&shx, self)),
                0x9F => yield_complete!(absolute_indexed::y_modify(&ahx, self)),
                0xA0 => immediate::read(&ldy, self),
                0xA1 => yield_complete!(indirect_indexed::x_read(&lda, self)),
                0xA2 => immediate::read(&ldx, self),
                0xA3 => yield_complete!(indirect_indexed::x_read(&lax, self)),
                0xA4 => yield_complete!(zero_page::read(&ldy, self)),
                0xA5 => yield_complete!(zero_page::read(&lda, self)),
                0xA6 => yield_complete!(zero_page::read(&ldx, self)),
                0xA7 => yield_complete!(zero_page::read(&lax, self)),
                0xA8 => implicit::run(&tay, self),
                0xA9 => immediate::read(&lda, self),
                0xAA => implicit::run(&tax, self),
                0xAB => immediate::read(&lax, self),
                0xAC => yield_complete!(absolute::read(&ldy, self)),
                0xAD => yield_complete!(absolute::read(&lda, self)),
                0xAE => yield_complete!(absolute::read(&ldx, self)),
                0xAF => yield_complete!(absolute::read(&lax, self)),
                0xB0 => yield_complete!(relative::branch(&bcs, self)),
                0xB1 => yield_complete!(indirect_indexed::y_read(&lda, self)),
                0xB2 => OpTrace::Implicit,
                0xB3 => yield_complete!(indirect_indexed::y_read(&lax, self)),
                0xB4 => yield_complete!(zero_page_indexed::x_read(&ldy, self)),
                0xB5 => yield_complete!(zero_page_indexed::x_read(&lda, self)),
                0xB6 => yield_complete!(zero_page_indexed::y_read(&ldx, self)),
                0xB7 => yield_complete!(zero_page_indexed::y_read(&lax, self)),
                0xB8 => implicit::run(&clv, self),
                0xB9 => yield_complete!(absolute_indexed::y_read(&lda, self)),
                0xBA => implicit::run(&tsx, self),
                0xBB => yield_complete!(absolute_indexed::y_read(&las, self)),
                0xBC => yield_complete!(absolute_indexed::x_read(&ldy, self)),
                0xBD => yield_complete!(absolute_indexed::x_read(&lda, self)),
                0xBE => yield_complete!(absolute_indexed::y_read(&ldx, self)),
                0xBF => yield_complete!(absolute_indexed::y_read(&lax, self)),
                0xC0 => immediate::read(&cpy, self),
                0xC1 => yield_complete!(indirect_indexed::x_read(&cmp, self)),
                0xC2 => immediate::read(&nop, self),
                0xC3 => yield_complete!(indirect_indexed::x_modify(&dcp, self)),
                0xC4 => yield_complete!(zero_page::read(&cpy, self)),
                0xC5 => yield_complete!(zero_page::read(&cmp, self)),
                0xC6 => yield_complete!(zero_page::modify(&dec, self)),
                0xC7 => yield_complete!(zero_page::modify(&dcp, self)),
                0xC8 => implicit::run(&iny, self),
                0xC9 => immediate::read(&cmp, self),
                0xCA => implicit::run(&dex, self),
                0xCB => immediate::read(&axs, self),
                0xCC => yield_complete!(absolute::read(&cpy, self)),
                0xCD => yield_complete!(absolute::read(&cmp, self)),
                0xCE => yield_complete!(absolute::modify(&dec, self)),
                0xCF => yield_complete!(absolute::modify(&dcp, self)),
                0xD0 => yield_complete!(relative::branch(&bne, self)),
                0xD1 => yield_complete!(indirect_indexed::y_read(&cmp, self)),
                0xD2 => OpTrace::Implicit,
                0xD3 => yield_complete!(indirect_indexed::y_modify(&dcp, self)),
                0xD4 => yield_complete!(zero_page_indexed::x_read(&nop, self)),
                0xD5 => yield_complete!(zero_page_indexed::x_read(&cmp, self)),
                0xD6 => yield_complete!(zero_page_indexed::x_modify(&dec, self)),
                0xD7 => yield_complete!(zero_page_indexed::x_modify(&dcp, self)),
                0xD8 => implicit::run(&cld, self),
                0xD9 => yield_complete!(absolute_indexed::y_read(&cmp, self)),
                0xDA => implicit::run(&nop, self),
                0xDB => yield_complete!(absolute_indexed::y_modify(&dcp, self)),
                0xDC => yield_complete!(absolute_indexed::x_read(&nop, self)),
                0xDD => yield_complete!(absolute_indexed::x_read(&cmp, self)),
                0xDE => yield_complete!(absolute_indexed::x_modify(&dec, self)),
                0xDF => yield_complete!(absolute_indexed::x_modify(&dcp, self)),
                0xE0 => immediate::read(&cpx, self),
                0xE1 => yield_complete!(indirect_indexed::x_read(&sbc, self)),
                0xE2 => immediate::read(&nop, self),
                0xE3 => yield_complete!(indirect_indexed::x_modify(&isc, self)),
                0xE4 => yield_complete!(zero_page::read(&cpx, self)),
                0xE5 => yield_complete!(zero_page::read(&sbc, self)),
                0xE6 => yield_complete!(zero_page::modify(&inc, self)),
                0xE7 => yield_complete!(zero_page::modify(&isc, self)),
                0xE8 => implicit::run(&inx, self),
                0xE9 => immediate::read(&sbc, self),
                0xEA => implicit::run(&nop, self),
                0xEB => immediate::read(&sbc, self),
                0xEC => yield_complete!(absolute::read(&cpx, self)),
                0xED => yield_complete!(absolute::read(&sbc, self)),
                0xEE => yield_complete!(absolute::modify(&inc, self)),
                0xEF => yield_complete!(absolute::modify(&isc, self)),
                0xF0 => yield_complete!(relative::branch(&beq, self)),
                0xF1 => yield_complete!(indirect_indexed::y_read(&sbc, self)),
                0xF2 => OpTrace::Implicit,
                0xF3 => yield_complete!(indirect_indexed::y_modify(&isc, self)),
                0xF4 => yield_complete!(zero_page_indexed::x_read(&nop, self)),
                0xF5 => yield_complete!(zero_page_indexed::x_read(&sbc, self)),
                0xF6 => yield_complete!(zero_page_indexed::x_modify(&inc, self)),
                0xF7 => yield_complete!(zero_page_indexed::x_modify(&isc, self)),
                0xF8 => implicit::run(&sed, self),
                0xF9 => yield_complete!(absolute_indexed::y_read(&sbc, self)),
                0xFA => implicit::run(&nop, self),
                0xFB => yield_complete!(absolute_indexed::y_modify(&isc, self)),
                0xFC => yield_complete!(absolute_indexed::x_read(&nop, self)),
                0xFD => yield_complete!(absolute_indexed::x_read(&sbc, self)),
                0xFE => yield_complete!(absolute_indexed::x_modify(&inc, self)),
                0xFF => yield_complete!(absolute_indexed::x_modify(&isc, self)),
                _ => {
                    println!("{:?}", &OPCODES[opcode as usize]);
                    unimplemented!()
                }
            };

            // poll the nmi line
            // TODO: this doesn't seem to be quite at the right spot...
            interrupt = self.bus.intr_latch();

            let opcode = &OPCODES[opcode as usize];
            match opcode.0 {
                Op::KIL => yield CpuCycle::Halt,
                _ => yield CpuCycle::OpComplete(opcode.clone(), trace),
            }
        }
    }

    pub fn decompile(addr: u16, addr_space: &dyn AddressSpace) -> (OpCode, Operand) {
        let opcode = OPCODES[addr_space.read_u8(addr) as usize].clone();
        let operand = opcode.1.operand(addr, addr_space);
        (opcode, operand)
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

pub struct CpuBus {
    pub ram: Ram2KB,
    pub ppu_registers: MemoryMappedRegisters,
    pub mapper: Rc<RefCell<Box<dyn Mapper>>>,
    pub intr: Cell<Option<Interrupt>>,
    oam_dma: Cell<Option<u8>>,
}

impl CpuBus {
    pub fn new(ppu_registers: MemoryMappedRegisters, mapper: Rc<RefCell<Box<dyn Mapper>>>) -> Self {
        CpuBus {
            ram: Ram2KB::new(),
            ppu_registers,
            mapper,
            intr: Cell::new(None),
            oam_dma: Cell::new(None),
        }
    }

    // This will return last write to OAM DMA and then None until the next write
    pub fn dma_latch(&self) -> Option<u8> {
        self.oam_dma.take()
    }

    pub fn intr_latch(&self) -> Option<Interrupt> {
        self.intr.take()
    }
}

impl crate::memory::AddressSpace for CpuBus {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram.read_u8(addr),
            0x0800..=0x1FFF => self.ram.read_u8(addr % 0x0800),

            0x2000..=0x2007 => self.ppu_registers.read_u8(addr), // PPU
            0x2008..=0x3FFF => self.ppu_registers.read_u8(0x2000 + (addr % 8)), // PPU mirror

            0x4000..=0x4017 => 0x00,             // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.mapper.borrow().cpu_addr_space().read_u8(addr), // PRG ROM/RAM and mapper
        }
    }

    fn write_u8(&self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x07FF => self.ram.write_u8(addr, value),
            0x0800..=0x1FFF => self.ram.write_u8(addr % 0x8000, value),

            0x2000..=0x2007 => self.ppu_registers.write_u8(addr, value), // PPU
            0x2008..=0x3FFF => self.ppu_registers.write_u8(0x2000 + (addr % 8), value), // PPU mirror

            0x4014 => self.oam_dma.set(Some(value)),

            0x4000..=0x4017 => (),               // APU
            0x4018..=0x401F => unimplemented!(), // APU and I/O functionality that is normally disabled.

            0x4020..=0xFFFF => self.mapper.borrow().cpu_addr_space().write_u8(addr, value), // PRG ROM/RAM and mapper
        }
    }
}
