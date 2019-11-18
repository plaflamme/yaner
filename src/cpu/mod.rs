use crate::memory::AddressSpace;

// http://obelisk.me.uk/6502/reference.html
#[derive(Debug, Clone, Copy)]
enum Op {
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

#[derive(Debug)]
enum AddressingMode {
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

// https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
// http://www.oxyron.de/html/opcodes02.html
#[derive(Debug)]
struct OpCode(Op, AddressingMode, u8, bool);

const OPCODES: [OpCode; 256] = {
    use Op::*;
    use AddressingMode::*;
    [
        // 0x
        OpCode(BRK, Implicit, 7, false), // x0
        OpCode(ORA, IndirectX, 6, false), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(SLO, IndirectX, 8, false), // x3
        OpCode(NOP, ZeroPage, 3, false), // x4
        OpCode(ORA, ZeroPage, 3, false), // x5
        OpCode(ASL, ZeroPage, 5, false), // x6
        OpCode(SLO, ZeroPage, 5, false), // x7
        OpCode(PHP, Implicit, 3, false), // x8
        OpCode(ORA, Immediate, 2, false), // x9
        OpCode(ASL, Implicit, 2, false), // xA
        OpCode(ANC, Immediate, 2, false), // xB
        OpCode(NOP, Absolute, 4, false), // xC
        OpCode(ORA, Absolute, 4, false), // xD
        OpCode(ASL, Absolute, 6, false), // xE
        OpCode(SLO, Absolute, 6, false), // xF
        // 1x
        OpCode(BPL, Relative, 2, true), // x0
        OpCode(ORA, IndirectY, 5, true), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(SLO, IndirectY, 8, false), // x3
        OpCode(NOP, ZeroPageX, 4, false), // x4
        OpCode(ORA, ZeroPageX, 4, false), // x5
        OpCode(ASL, ZeroPageX, 6, false), // x6
        OpCode(SLO, ZeroPageX, 6, false), // x7
        OpCode(CLC, Implicit, 2, false), // x8
        OpCode(ORA, AbsoluteY, 4, true), // x9
        OpCode(NOP, Implicit, 2, false), // xA
        OpCode(SLO, AbsoluteY, 7, false), // xB
        OpCode(NOP, AbsoluteX, 4, true), // xC
        OpCode(ORA, AbsoluteX, 4, true), // xD
        OpCode(ASL, AbsoluteX, 7, false), // xE
        OpCode(SLO, AbsoluteX, 7, false), // xF
        // 2x
        OpCode(JSR, Absolute, 6, false), // x0
        OpCode(AND, IndirectX, 6, false), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(RLA, IndirectX, 8, false), // x3
        OpCode(BIT, ZeroPage, 3, false), // x4
        OpCode(AND, ZeroPage, 3, false), // x5
        OpCode(ROL, ZeroPage, 5, false), // x6
        OpCode(RLA, ZeroPage, 5, false), // x7
        OpCode(PLP, Implicit, 4, false), // x8
        OpCode(AND, Immediate, 2, false), // x9
        OpCode(ROL, Implicit, 2, false), // xA
        OpCode(ANC, Immediate, 2, false), // xB
        OpCode(BIT, Absolute, 4, false), // xC
        OpCode(AND, Absolute, 4, false), // xD
        OpCode(ROL, Absolute, 6, false), // xE
        OpCode(RLA, Absolute, 6, false), // xF
        // 3x
        OpCode(BMI, Relative, 2, true), // x0
        OpCode(AND, IndirectY, 5, true), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(RLA, IndirectY, 8, false), // x3
        OpCode(NOP, ZeroPageX, 4, false), // x4
        OpCode(AND, ZeroPageX, 4, false), // x5
        OpCode(ROL, ZeroPageX, 6, false), // x6
        OpCode(RLA, ZeroPageX, 6, false), // x7
        OpCode(SEC, Implicit, 2, false), // x8
        OpCode(AND, AbsoluteY, 4, true), // x9
        OpCode(NOP, Implicit, 2, false), // xA
        OpCode(RLA, AbsoluteY, 7, false), // xB
        OpCode(NOP, AbsoluteX, 4, true), // xC
        OpCode(AND, AbsoluteX, 4, true), // xD
        OpCode(ROL, AbsoluteX, 7, false), // xE
        OpCode(RLA, AbsoluteX, 7, false), // xF
        // 4x
        OpCode(RTI, Implicit, 6, false), // x0
        OpCode(EOR, IndirectX, 6, false), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(SRE, IndirectX, 8, false), // x3
        OpCode(NOP, ZeroPage, 3, false), // x4
        OpCode(EOR, ZeroPage, 3, false), // x5
        OpCode(LSR, ZeroPage, 5, false), // x6
        OpCode(SRE, ZeroPage, 5, false), // x7
        OpCode(PHA, Implicit, 3, false), // x8
        OpCode(EOR, Immediate, 2, false), // x9
        OpCode(LSR, Implicit, 2, false), // xA
        OpCode(ALR, Immediate, 2, false), // xB
        OpCode(JMP, Absolute, 3, false), // xC
        OpCode(EOR, Absolute, 4, false), // xD
        OpCode(LSR, Absolute, 6, false), // xE
        OpCode(SRE, Absolute, 6, false), // xF
        // 5x
        OpCode(BVC, Relative, 2, true), // x0
        OpCode(EOR, IndirectY, 5, true), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(SRE, IndirectY, 8, false), // x3
        OpCode(NOP, ZeroPageX, 4, false), // x4
        OpCode(EOR, ZeroPageX, 4, false), // x5
        OpCode(LSR, ZeroPageX, 6, false), // x6
        OpCode(SRE, ZeroPageX, 6, false), // x7
        OpCode(CLI, Implicit, 2, false), // x8
        OpCode(EOR, AbsoluteY, 4, true), // x9
        OpCode(NOP, Implicit, 2, false), // xA
        OpCode(SRE, AbsoluteY, 7, false), // xB
        OpCode(NOP, AbsoluteX, 4, true), // xC
        OpCode(EOR, AbsoluteX, 4, true), // xD
        OpCode(LSR, AbsoluteX, 7, false), // xE
        OpCode(SRE, AbsoluteX, 7, false), // xF
        // 6x
        OpCode(RTS, Implicit, 6, false), // x0
        OpCode(ADC, IndirectX, 6, false), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(RRA, IndirectX, 8, false), // x3
        OpCode(NOP, ZeroPage, 3, false), // x4
        OpCode(ADC, ZeroPage, 3, false), // x5
        OpCode(ROR, ZeroPage, 5, false), // x6
        OpCode(RRA, ZeroPage, 5, false), // x7
        OpCode(PLA, Implicit, 4, false), // x8
        OpCode(ADC, Immediate, 2, false), // x9
        OpCode(ROR, Implicit, 2, false), // xA
        OpCode(ARR, Immediate, 2, false), // xB
        OpCode(JMP, Indirect, 5, false), // xC
        OpCode(ADC, Absolute, 4, false), // xD
        OpCode(ROR, Absolute, 6, false), // xE
        OpCode(RRA, Absolute, 6, false), // xF
        // 7x
        OpCode(BVS, Relative, 2, true), // x0
        OpCode(ADC, IndirectY, 5, true), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(RRA, IndirectY, 8, false), // x3
        OpCode(NOP, ZeroPageX, 4, false), // x4
        OpCode(ADC, ZeroPageX, 4, false), // x5
        OpCode(ROR, ZeroPageX, 6, false), // x6
        OpCode(RRA, ZeroPageX, 6, false), // x7
        OpCode(SEI, Implicit, 2, false), // x8
        OpCode(ADC, AbsoluteY, 4, true), // x9
        OpCode(NOP, Implicit, 2, false), // xA
        OpCode(RRA, AbsoluteY, 7, false), // xB
        OpCode(NOP, AbsoluteX, 4, true), // xC
        OpCode(ADC, AbsoluteX, 4, true), // xD
        OpCode(ROR, AbsoluteX, 7, false), // xE
        OpCode(RRA, AbsoluteX, 7, false), // xF
        // 8x
        OpCode(NOP, Immediate, 2, false), // x0
        OpCode(STA, IndirectX, 6, false), // x1
        OpCode(NOP, Immediate, 2, false), // x2
        OpCode(SAX, IndirectX, 6, false), // x3
        OpCode(STY, ZeroPage, 3, false), // x4
        OpCode(STA, ZeroPage, 3, false), // x5
        OpCode(STX, ZeroPage, 3, false), // x6
        OpCode(SAX, ZeroPage, 3, false), // x7
        OpCode(DEY, Implicit, 2, false), // x8
        OpCode(NOP, Immediate, 2, false), // x9
        OpCode(TXA, Implicit, 2, false), // xA
        OpCode(XAA, Immediate, 2, false), // xB
        OpCode(STY, Absolute, 4, false), // xC
        OpCode(STA, Absolute, 4, false), // xD
        OpCode(STX, Absolute, 4, false), // xE
        OpCode(SAX, Absolute, 4, false), // xF
        // 9x
        OpCode(BCC, Relative, 2, true), // x0
        OpCode(STA, IndirectY, 6, false), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(AHX, IndirectY, 6, false), // x3
        OpCode(STY, ZeroPageX, 4, false), // x4
        OpCode(STA, ZeroPageX, 4, false), // x5
        OpCode(STX, ZeroPageY, 4, false), // x6
        OpCode(SAX, ZeroPageY, 4, false), // x7
        OpCode(TYA, Implicit, 2, false), // x8
        OpCode(STA, AbsoluteY, 5, false), // x9
        OpCode(TXS, Implicit, 2, false), // xA
        OpCode(TAS, AbsoluteY, 5, false), // xB
        OpCode(SHY, AbsoluteX, 5, false), // xC
        OpCode(STA, AbsoluteX, 5, false), // xD
        OpCode(SHX, AbsoluteY, 5, false), // xE
        OpCode(AHX, AbsoluteY, 5, false), // xF
        // Ax
        OpCode(LDY, Immediate, 2, false), // x0
        OpCode(LDA, IndirectX, 6, false), // x1
        OpCode(LDX, Immediate, 2, false), // x2
        OpCode(LAX, IndirectX, 6, false), // x3
        OpCode(LDY, ZeroPage, 3, false), // x4
        OpCode(LDA, ZeroPage, 3, false), // x5
        OpCode(LDX, ZeroPage, 3, false), // x6
        OpCode(LAX, ZeroPage, 3, false), // x7
        OpCode(TAY, Implicit, 2, false), // x8
        OpCode(LDA, Immediate, 2, false), // x9
        OpCode(TAX, Implicit, 2, false), // xA
        OpCode(LAX, Immediate, 2, false), // xB
        OpCode(LDY, Absolute, 4, false), // xC
        OpCode(LDA, Absolute, 4, false), // xD
        OpCode(LDX, Absolute, 4, false), // xE
        OpCode(LAX, Absolute, 4, false), // xF
        // Bx
        OpCode(BCS, Relative, 2, true), // x0
        OpCode(LDA, IndirectY, 5, true), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(LAX, IndirectY, 5, true), // x3
        OpCode(LDY, ZeroPageX, 4, false), // x4
        OpCode(LDA, ZeroPageX, 4, false), // x5
        OpCode(LDX, ZeroPageY, 4, false), // x6
        OpCode(LAX, ZeroPageY, 4, false), // x7
        OpCode(CLV, Implicit, 2, false), // x8
        OpCode(LDA, AbsoluteY, 4, true), // x9
        OpCode(TSX, Implicit, 2, false), // xA
        OpCode(LAS, AbsoluteY, 4, true), // xB
        OpCode(LDY, AbsoluteX, 4, true), // xC
        OpCode(LDA, AbsoluteX, 4, true), // xD
        OpCode(LDX, AbsoluteY, 4, true), // xE
        OpCode(LAX, AbsoluteY, 4, true), // xF
        // Cx
        OpCode(CPY, Immediate, 2, false), // x0
        OpCode(CMP, IndirectX, 6, false), // x1
        OpCode(NOP, Immediate, 2, false), // x2
        OpCode(DCP, IndirectX, 8, false), // x3
        OpCode(CPY, ZeroPage, 3, false), // x4
        OpCode(CMP, ZeroPage, 3, false), // x5
        OpCode(DEC, ZeroPage, 5, false), // x6
        OpCode(DCP, ZeroPage, 5, false), // x7
        OpCode(INY, Implicit, 2, false), // x8
        OpCode(CMP, Immediate, 2, false), // x9
        OpCode(DEX, Implicit, 2, false), // xA
        OpCode(AXS, Immediate, 2, false), // xB
        OpCode(CPY, Absolute, 4, false), // xC
        OpCode(CMP, Absolute, 4, false), // xD
        OpCode(DEC, Absolute, 6, false), // xE
        OpCode(DCP, Absolute, 6, false), // xF
        // Dx
        OpCode(BNE, Relative, 2, true), // x0
        OpCode(CMP, IndirectY, 5, true), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(DCP, IndirectY, 8, false), // x3
        OpCode(NOP, ZeroPageX, 4, false), // x4
        OpCode(CMP, ZeroPageX, 4, false), // x5
        OpCode(DEC, ZeroPageX, 6, false), // x6
        OpCode(DCP, ZeroPageX, 6, false), // x7
        OpCode(CLD, Implicit, 2, false), // x8
        OpCode(CMP, AbsoluteY, 4, true), // x9
        OpCode(NOP, Implicit, 2, false), // xA
        OpCode(DCP, AbsoluteY, 7, false), // xB
        OpCode(NOP, AbsoluteX, 4, true), // xC
        OpCode(CMP, AbsoluteX, 4, true), // xD
        OpCode(DEC, AbsoluteX, 7, false), // xE
        OpCode(DCP, AbsoluteX, 7, false), // xF
        // Ex
        OpCode(CPX, Immediate, 2, false), // x0
        OpCode(SBC, IndirectX, 6, false), // x1
        OpCode(NOP, Immediate, 2, false), // x2
        OpCode(ISC, IndirectX, 8, false), // x3
        OpCode(CPX, ZeroPage, 3, false), // x4
        OpCode(SBC, ZeroPage, 3, false), // x5
        OpCode(INC, ZeroPage, 5, false), // x6
        OpCode(ISC, ZeroPage, 5, false), // x7
        OpCode(INX, Implicit, 2, false), // x8
        OpCode(SBC, Immediate, 2, false), // x9
        OpCode(NOP, Implicit, 2, false), // xA
        OpCode(SBC, Immediate, 2, false), // xB
        OpCode(CPX, Absolute, 4, false), // xC
        OpCode(SBC, Absolute, 4, false), // xD
        OpCode(INC, Absolute, 6, false), // xE
        OpCode(ISC, Absolute, 6, false), // xF
        // Fx
        OpCode(BEQ, Relative, 2, true), // x0
        OpCode(SBC, IndirectY, 5, true), // x1
        OpCode(KIL, Implicit, 0, false), // x2
        OpCode(ISC, IndirectY, 8, false), // x3
        OpCode(NOP, ZeroPageX, 4, false), // x4
        OpCode(SBC, ZeroPageX, 4, false), // x5
        OpCode(INC, ZeroPageX, 6, false), // x6
        OpCode(ISC, ZeroPageX, 6, false), // x7
        OpCode(SED, Implicit, 2, false), // x8
        OpCode(SBC, AbsoluteY, 4, true), // x9
        OpCode(NOP, Implicit, 2, false), // xA
        OpCode(ISC, AbsoluteY, 7, false), // xB
        OpCode(NOP, AbsoluteX, 4, true), // xC
        OpCode(SBC, AbsoluteX, 4, true), // xD
        OpCode(INC, AbsoluteX, 7, false), // xE
        OpCode(ISC, AbsoluteX, 7, false), // xF
    ]
};

// http://wiki.nesdev.com/w/index.php/Status_flags
struct Flags {
    negative: bool,
    overflow: bool,
    break_: bool,
    decimal: bool,
    interrupt: bool,
    zero: bool,
    carry: bool
}

impl Flags {
    fn new() -> Self {
        Flags { negative: false, overflow: false, break_: false, decimal: false, interrupt: false, zero: false, carry: false }
    }
}

impl Into<u8> for &Flags {
    fn into(self) -> u8 {
        unimplemented!()
    }
}

impl From<u8> for Flags {
    fn from(_: u8) -> Self {
        unimplemented!()
    }
}

// http://nesdev.com/6502_cpu.txt
pub struct RP2A03 {
    acc: u8,
    x: u8,
    y: u8,
    flags: Flags,
    sp: u8,
    pc: u16,

    mem_map: Box<dyn crate::memory::AddressSpace>
}

impl RP2A03 {
    fn new(memory_map: Box<dyn AddressSpace>) -> Self {
        RP2A03 { acc: 0, x: 0, y: 0, flags: Flags::new(), sp: 0, pc: 0, mem_map: memory_map }
    }

    fn tick(&mut self) {
        let opcode_value = self.mem_map.peek(self.pc);
        let opcode = &OPCODES[opcode_value as usize];
        self.pc = self.pc.wrapping_add(1);
        let (value, addr) = match opcode.1 {
            AddressingMode::Immediate =>
                (Some(self.mem_map.peek(self.pc) as u16), None),
            AddressingMode::ZeroPage => {
                let addr = self.mem_map.peek(self.pc) as u16;
                (Some(self.mem_map.peek(addr) as u16), Some(addr))
            },
            AddressingMode::ZeroPageX => {
                let addr = self.mem_map.peek(self.pc).wrapping_add(self.x) as u16;
                (Some(self.mem_map.peek(addr) as u16), Some(addr))
            },
            AddressingMode::ZeroPageY =>  {
                let addr = self.mem_map.peek(self.pc).wrapping_add(self.y) as u16;
                (Some(self.mem_map.peek(addr) as u16), Some(addr))
            },

            AddressingMode::Absolute => {
                let addr = self.mem_map.peek16(self.pc);
                (Some(self.mem_map.peek(addr) as u16), Some(addr))
            },
            AddressingMode::AbsoluteX => {
                let addr = self.mem_map.peek16(self.pc).wrapping_add(self.x as u16);
                (Some(self.mem_map.peek(addr) as u16), Some(addr))
            },
            AddressingMode::AbsoluteY => {
                let addr = self.mem_map.peek16(self.pc).wrapping_add(self.y as u16);
                (Some(self.mem_map.peek(addr) as u16), Some(addr))
            },

            AddressingMode::Indirect => {
                // this is only used by JMP and its behaviour was actually buggy.
                //   if addr is 0x01FF, it would fetch the lsb from 0x01FF, but msb at 0x0100 instead of 0x0200
                let addr = self.mem_map.peek16(self.pc);
                let lsb = self.mem_map.peek(addr) as u16;
                let msb_addr = if addr & 0xFF != 0xFF { addr.wrapping_add(1) } else { addr & 0xFF00 };
                let msb = self.mem_map.peek(msb_addr) as u16;
                let v = (msb << 8) | lsb;
                (Some(v), Some(addr))
            },
            AddressingMode::IndirectX => {
                // val = PEEK(PEEK((arg + X) % 256) + PEEK((arg + X + 1) % 256) * 256)
                let addr = self.mem_map.peek(self.pc).wrapping_add(self.x);
                let v = self.mem_map.peek16(addr as u16);
                (Some(v), Some(addr as u16))
            },
            AddressingMode::IndirectY => {
                // val = PEEK(PEEK(arg) + PEEK((arg + 1) % 256) * 256 + Y)
                let addr = self.mem_map.peek(self.pc);
                let v = self.mem_map.peek16(addr as u16).wrapping_add(self.y as u16);
                (Some(v), Some(addr as u16))
            },

            AddressingMode::Accumulator => (Some(self.acc as u16), None),
            AddressingMode::Relative => (Some(self.mem_map.peek(self.pc) as u16), None),

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
                self.mem_map.store(addr, result);
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
                self.mem_map.store(addr, result);
                self.set_flags_from(result);
            },
            (Op::DEX, None, _) => self.dex(),
            (Op::DEY, None, _) => self.dey(),

            (Op::EOR, Some(v), _) => self.eor(v as u8),

            (Op::INC, Some(v), Some(addr)) => {
                let result = self.inc(v as u8);
                self.mem_map.store(addr, result);
                self.set_flags_from(result);
            },
            (Op::INX, None, _) => self.inx(),
            (Op::INY, None, _) => self.iny(),

            (Op::JMP, Some(addr), _) => self.jmp(addr),
            (Op::JSR, Some(v), _) => self.jsr(v),

            _ => unimplemented!()
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

    // sets the negative and zero flags
    fn set_flags_from(&mut self, v: u8) {
        self.flags.negative = (v & 0x80) != 0;
        self.flags.zero = v == 0;
    }

    fn set_flags_from_acc(&mut self) {
        self.set_flags_from(self.acc)
    }
}

// Operations
impl RP2A03 {

    // http://obelisk.me.uk/6502/reference.html#ADC
    fn adc(&mut self, v: u8) {
        let (v1, o1) = self.acc.overflowing_add(v);
        let (v2, o2) = v1.overflowing_add(self.flags.carry as u8);

        self.flags.carry = o1 | o2;
        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        self.flags.overflow = (v^v2) & (self.acc^v2) & 0x80 != 0;
        self.acc = v2;
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#AND
    fn and(&mut self, v: u8) {
        self.acc = self.acc & v;
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#ASL
    // NOTE: this can apply to the accumulator or some memory location
    fn asl(&mut self, v: u8) -> u8 {
        let mult = v << 1;
        self.flags.carry = (v & 0x80) != 0;
        mult
    }

    fn branch_if(&mut self, branch: bool, v: u8) {
        if branch {
            // TODO: +1 cycle
            // relative displacement, v is signed in this case
            self.pc = self.pc.wrapping_add(v as i8 as u16);
            // TODO: +1 if branching to a new page
        }
    }

    // http://obelisk.me.uk/6502/reference.html#BCC
    fn bcc(&mut self, v: u8) {
        self.branch_if(!self.flags.carry, v)
    }

    // http://obelisk.me.uk/6502/reference.html#BCS
    fn bcs(&mut self, v: u8) {
        self.branch_if(self.flags.carry, v)
    }

    // http://obelisk.me.uk/6502/reference.html#BEQ
    fn beq(&mut self, v: u8) {
        self.branch_if(self.flags.zero, v)
    }

    // http://obelisk.me.uk/6502/reference.html#BIT
    fn bit(&mut self, v: u8) {
        let r = self.acc & v;
        self.flags.zero = r == 0;
        self.flags.overflow = (v & 0x40) != 0; // set to the 6th bit of the value
        self.flags.negative = (v & 0x80) != 0; // set to the 7th bit of the value
    }

    // http://obelisk.me.uk/6502/reference.html#BMI
    fn bmi(&mut self, v: u8) {
        self.branch_if(self.flags.negative, v)
    }

    // http://obelisk.me.uk/6502/reference.html#BNE
    fn bne(&mut self, v: u8) {
        self.branch_if(!self.flags.zero, v)
    }

    // http://obelisk.me.uk/6502/reference.html#BPL
    fn bpl(&mut self, v: u8) {
        self.branch_if(!self.flags.negative, v)
    }

    // http://obelisk.me.uk/6502/reference.html#BRK
    fn brk(&mut self) {
        self.push_stack16(self.pc);
        self.push_stack((&self.flags).into());
        self.pc = self.mem_map.peek16(0xFFFE);
    }

    // http://obelisk.me.uk/6502/reference.html#BVC
    fn bvc(&mut self, v: u8) {
        self.branch_if(!self.flags.overflow, v)
    }

    // http://obelisk.me.uk/6502/reference.html#BVS
    fn bvs(&mut self, v: u8) {
        self.branch_if(self.flags.overflow, v)
    }

    // http://obelisk.me.uk/6502/reference.html#CLC
    fn clc(&mut self) {
        self.flags.carry = false
    }

    // http://obelisk.me.uk/6502/reference.html#CLD
    fn cld(&mut self) {
        self.flags.decimal = false
    }

    // http://obelisk.me.uk/6502/reference.html#CLI
    fn cli(&mut self) {
        self.flags.interrupt = false
    }

    // http://obelisk.me.uk/6502/reference.html#CLV
    fn clv(&mut self) {
        self.flags.overflow = false
    }

    fn compare(&mut self, a: u8, b: u8) {
        let result = a - b;
        self.flags.carry = a >= b;
        self.flags.zero = a == b;
        self.flags.negative = (result & 0x80) > 0;
    }

    // http://obelisk.me.uk/6502/reference.html#CLV
    fn cmp(&mut self, v: u8) {
        self.compare(self.acc, v)
    }

    // http://obelisk.me.uk/6502/reference.html#CPX
    fn cpx(&mut self, v: u8) {
        self.compare(self.x, v)
    }

    // http://obelisk.me.uk/6502/reference.html#CPY
    fn cpy(&mut self, v: u8) {
        self.compare(self.y, v)
    }

    // http://obelisk.me.uk/6502/reference.html#DEC
    fn dec(&mut self, v: u8) -> u8 {
        let result = v.wrapping_sub(1);
        self.set_flags_from(result);
        result
    }

    // http://obelisk.me.uk/6502/reference.html#DEX
    fn dex(&mut self) {
        self.x = self.dec(self.x);
    }

    // http://obelisk.me.uk/6502/reference.html#DEY
    fn dey(&mut self) {
        self.y = self.dec(self.y);
    }

    // http://obelisk.me.uk/6502/reference.html#EOR
    fn eor(&mut self, v: u8) {
        self.acc ^= v;
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#INC
    fn inc(&mut self, v: u8) -> u8 {
        let result = v.wrapping_add(1);
        self.set_flags_from(result);
        result
    }

    // http://obelisk.me.uk/6502/reference.html#INX
    fn inx(&mut self) {
        self.x = self.inc(self.x);
    }

    // http://obelisk.me.uk/6502/reference.html#INY
    fn iny(&mut self) {
        self.y = self.inc(self.y);
    }

    // http://obelisk.me.uk/6502/reference.html#JMP
    fn jmp(&mut self, v: u16) {
        self.pc = v;
    }

    // http://obelisk.me.uk/6502/reference.html#JSR
    fn jsr(&mut self, v: u16) {
        self.push_stack16(self.pc.wrapping_sub(1));
        self.jmp(v);
    }

    // http://obelisk.me.uk/6502/reference.html#LDA
    fn lda(&mut self, v: u8) {
        self.acc = v;
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#LDX
    fn ldx(&mut self, v: u8) {
        self.x = v;
        self.set_flags_from(self.x);
    }

    // http://obelisk.me.uk/6502/reference.html#LDY
    fn ldy(&mut self, v: u8) {
        self.y = v;
        self.set_flags_from(self.y);
    }

    // http://obelisk.me.uk/6502/reference.html#LSR
    fn lsr(&mut self, v: u8) -> u8 {
        self.flags.carry = (v & 0x01) != 0;
        let result = v >> 1;
        self.set_flags_from(result);
        result
    }

    // http://obelisk.me.uk/6502/reference.html#NOP
    fn nop(&mut self) {
    }

    // http://obelisk.me.uk/6502/reference.html#ORA
    fn ora(&mut self, v: u8) {
        self.acc |= v;
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#PHA
    fn pha(&mut self) {
        self.push_stack((&self.flags).into());
    }

    // http://obelisk.me.uk/6502/reference.html#PLA
    fn pla(&mut self) {
        self.acc = self.pop_stack();
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#PLP
    fn plp(&mut self) {
        self.flags = self.pop_stack().into();
    }

    // http://obelisk.me.uk/6502/reference.html#ROL
    fn rol(&mut self, v: u8) -> u8 {
        let result = (v << 1) | self.flags.carry as u8;
        self.flags.carry = v & 0x80 != 0;
        self.set_flags_from(result);
        result
    }

    // http://obelisk.me.uk/6502/reference.html#ROR
    fn ror(&mut self, v: u8) -> u8 {
        let result = (v >> 1) | ((self.flags.carry as u8) << 7);
        self.flags.carry = v & 0x01 != 0;
        self.set_flags_from(result);
        result
    }

    // http://obelisk.me.uk/6502/reference.html#RTI
    fn rti(&mut self) {
        self.flags = self.pop_stack().into();
        self.pc = self.pop_stack16();
    }

    // http://obelisk.me.uk/6502/reference.html#RTS
    fn rts(&mut self) {
        self.pc = self.pop_stack16().wrapping_add(1);
    }

    // http://obelisk.me.uk/6502/reference.html#SBC
    fn sbc(&mut self, v: u8) {
        let (v1, o1) = self.acc.overflowing_sub(v);
        let (v2, o2) = v1.overflowing_sub(!self.flags.carry as u8);
        self.flags.carry = o1 | o2;
        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        self.flags.overflow = (v^v2) & (self.acc^v2) & 0x80 != 0;
        self.acc = v2;
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#SEC
    fn sec(&mut self) {
        self.flags.carry = true
    }

    // http://obelisk.me.uk/6502/reference.html#SED
    fn sed(&mut self) {
        self.flags.decimal = true
    }

    // http://obelisk.me.uk/6502/reference.html#SEI
    fn sei(&mut self) {
        self.flags.interrupt = true
    }

    // http://obelisk.me.uk/6502/reference.html#STA
    fn sta(&mut self, v: u16) {
        self.mem_map.store(v, self.acc);
    }

    // http://obelisk.me.uk/6502/reference.html#STX
    fn stx(&mut self, v: u16) {
        self.mem_map.store(v, self.x);
    }

    // http://obelisk.me.uk/6502/reference.html#STY
    fn sty(&mut self, v: u16) {
        self.mem_map.store(v, self.y);
    }

    // http://obelisk.me.uk/6502/reference.html#TAX
    fn tax(&mut self) {
        self.x = self.acc;
        self.set_flags_from(self.x);
    }

    // http://obelisk.me.uk/6502/reference.html#TAY
    fn tay(&mut self) {
        self.y = self.acc;
        self.set_flags_from(self.y);
    }

    // http://obelisk.me.uk/6502/reference.html#TSX
    fn tsx(&mut self) {
        self.x = self.sp;
        self.set_flags_from(self.x);
    }

    // http://obelisk.me.uk/6502/reference.html#TXA
    fn txa(&mut self) {
        self.acc = self.x;
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#TXS
    fn txs(&mut self) {
        self.sp = self.x;
        self.set_flags_from_acc();
    }

    // http://obelisk.me.uk/6502/reference.html#TYA
    fn tya(&mut self) {
        self.acc = self.y;
        self.set_flags_from_acc();
    }

}

pub mod generator {
    use super::*;
    use super::Op::*;
    use super::AddressingMode::*;

    const RAW_CODES: &str = r#"
0x|BRK7|ORAizx 6|KIL|SLOizx 8|NOPzp 3|ORAzp 3|ASLzp 5|SLOzp 5|PHP3|ORAimm 2|ASL2|ANCimm 2|NOPabs 4|ORAabs 4|ASLabs 6|SLOabs 6|
1x|BPLrel 2*|ORAizy 5*|KIL|SLOizy 8|NOPzpx 4|ORAzpx 4|ASLzpx 6|SLOzpx 6|CLC2|ORAaby 4*|NOP2|SLOaby 7|NOPabx 4*|ORAabx 4*|ASLabx 7|SLOabx 7|
2x|JSRabs 6|ANDizx 6|KIL|RLAizx 8|BITzp 3|ANDzp 3|ROLzp 5|RLAzp 5|PLP4|ANDimm 2|ROL2|ANCimm 2|BITabs 4|ANDabs 4|ROLabs 6|RLAabs 6|
3x|BMIrel 2*|ANDizy 5*|KIL|RLAizy 8|NOPzpx 4|ANDzpx 4|ROLzpx 6|RLAzpx 6|SEC2|ANDaby 4*|NOP2|RLAaby 7|NOPabx 4*|ANDabx 4*|ROLabx 7|RLAabx 7|
4x|RTI6|EORizx 6|KIL|SREizx 8|NOPzp 3|EORzp 3|LSRzp 5|SREzp 5|PHA3|EORimm 2|LSR2|ALRimm 2|JMPabs 3|EORabs 4|LSRabs 6|SREabs 6|
5x|BVCrel 2*|EORizy 5*|KIL|SREizy 8|NOPzpx 4|EORzpx 4|LSRzpx 6|SREzpx 6|CLI2|EORaby 4*|NOP2|SREaby 7|NOPabx 4*|EORabx 4*|LSRabx 7|SREabx 7|
6x|RTS6|ADCizx 6|KIL|RRAizx 8|NOPzp 3|ADCzp 3|RORzp 5|RRAzp 5|PLA4|ADCimm 2|ROR2|ARRimm 2|JMPind 5|ADCabs 4|RORabs 6|RRAabs 6|
7x|BVSrel 2*|ADCizy 5*|KIL|RRAizy 8|NOPzpx 4|ADCzpx 4|RORzpx 6|RRAzpx 6|SEI2|ADCaby 4*|NOP2|RRAaby 7|NOPabx 4*|ADCabx 4*|RORabx 7|RRAabx 7|
8x|NOPimm 2|STAizx 6|NOPimm 2|SAXizx 6|STYzp 3|STAzp 3|STXzp 3|SAXzp 3|DEY2|NOPimm 2|TXA2|XAAimm 2|STYabs 4|STAabs 4|STXabs 4|SAXabs 4|
9x|BCCrel 2*|STAizy 6|KIL|AHXizy 6|STYzpx 4|STAzpx 4|STXzpy 4|SAXzpy 4|TYA2|STAaby 5|TXS2|TASaby 5|SHYabx 5|STAabx 5|SHXaby 5|AHXaby 5|
Ax|LDYimm 2|LDAizx 6|LDXimm 2|LAXizx 6|LDYzp 3|LDAzp 3|LDXzp 3|LAXzp 3|TAY2|LDAimm 2|TAX2|LAXimm 2|LDYabs 4|LDAabs 4|LDXabs 4|LAXabs 4|
Bx|BCSrel 2*|LDAizy 5*|KIL|LAXizy 5*|LDYzpx 4|LDAzpx 4|LDXzpy 4|LAXzpy 4|CLV2|LDAaby 4*|TSX2|LASaby 4*|LDYabx 4*|LDAabx 4*|LDXaby 4*|LAXaby 4*|
Cx|CPYimm 2|CMPizx 6|NOPimm 2|DCPizx 8|CPYzp 3|CMPzp 3|DECzp 5|DCPzp 5|INY2|CMPimm 2|DEX2|AXSimm 2|CPYabs 4|CMPabs 4|DECabs 6|DCPabs 6|
Dx|BNErel 2*|CMPizy 5*|KIL|DCPizy 8|NOPzpx 4|CMPzpx 4|DECzpx 6|DCPzpx 6|CLD2|CMPaby 4*|NOP2|DCPaby 7|NOPabx 4*|CMPabx 4*|DECabx 7|DCPabx 7|
Ex|CPXimm 2|SBCizx 6|NOPimm 2|ISCizx 8|CPXzp 3|SBCzp 3|INCzp 5|ISCzp 5|INX2|SBCimm 2|NOP2|SBCimm 2|CPXabs 4|SBCabs 4|INCabs 6|ISCabs 6|
Fx|BEQrel 2*|SBCizy 5*|KIL|ISCizy 8|NOPzpx 4|SBCzpx 4|INCzpx 6|ISCzpx 6|SED2|SBCaby 4*|NOP2|ISCaby 7|NOPabx 4*|SBCabx 4*|INCabx 7|ISCabx 7|
"#;

    pub fn generate_opcode_table() {
        use std::str::FromStr;

        fn parse_code(code: &str) -> (Op, AddressingMode) {
            let (op_str, addr_str) = code.split_at(3);
            let op = match op_str {
                "ADC" => ADC, "AHX" => AHX, "ANC" => ANC, "AND" => AND,
                "ALR" => ALR, "ARR" => ARR, "ASL" => ASL, "AXS" => AXS,
                "BCC" => BCC, "BCS" => BCS, "BEQ" => BEQ,
                "BIT" => BIT, "BMI" => BMI, "BNE" => BNE,
                "BPL" => BPL, "BRK" => BRK, "BVC" => BVC,
                "BVS" => BVS, "CLC" => CLC, "CLD" => CLD,
                "CLI" => CLI, "CLV" => CLV, "CMP" => CMP,
                "CPX" => CPX, "CPY" => CPY, "DCP" => DCP, "DEC" => DEC,
                "DEX" => DEX, "DEY" => DEY, "EOR" => EOR,
                "INC" => INC, "INX" => INX, "INY" => INY, "ISC" => ISC,
                "JMP" => JMP, "JSR" => JSR, "KIL" => KIL,
                "LAS" => LAS, "LAX" => LAX,
                "LDA" => LDA, "LDX" => LDX, "LDY" => LDY,
                "LSR" => LSR, "NOP" => NOP, "ORA" => ORA,
                "PHA" => PHA, "PHP" => PHP, "PLA" => PLA,
                "PLP" => PLP, "RLA" => RLA, "ROL" => ROL, "ROR" => ROR, "RRA" => RRA,
                "RTI" => RTI, "RTS" => RTS, "SAX" => SAX, "SBC" => SBC, "SHX" => SHX, "SHY" => SHY,
                "SEC" => SEC, "SED" => SED, "SEI" => SEI,
                "SLO" => SLO, "SRE" => SRE,
                "STA" => STA, "STX" => STX, "STY" => STY, "TAS" => TAS,
                "TAX" => TAX, "TAY" => TAY, "TSX" => TSX,
                "TXA" => TXA, "TXS" => TXS, "TYA" => TYA, "XAA" => XAA,
                c => panic!("Unexpected code {}", c)
            };

            let addr = match addr_str {
                "zp" => ZeroPage, "zpx" => ZeroPageX, "zpy" => ZeroPageY,
                "ind" => Indirect, "izx" => IndirectX, "izy" => IndirectY,
                "abs" => Absolute, "abx" => AbsoluteX, "aby" => AbsoluteY,
                "imm" => Immediate, "rel" => Relative, "" => Implicit,
                c => panic!("Unexpected addressing mode {}", c)
            };

            (op, addr)
        }

        fn parse_opcode(code: &str) -> OpCode {
            let (code_str, cycles_str) = match code.len() {
                3 => {
                    (code, "") // KIL
                }
                4 => {
                    code.split_at(3) // BRK7
                },
                _ => {
                    // ORAzp 7
                    let parts = code.split_ascii_whitespace().collect::<Vec<_>>();
                    (parts[0], parts[1])
                }
            };

            let (op, addr) = parse_code(code_str);

            let cycles = if cycles_str.len() > 0 {
                u8::from_str(&cycles_str.replace('*', "")).unwrap()
            } else { 0 };
            OpCode(op, addr, cycles, cycles_str.contains('*'))
        }

        println!("[");
        RAW_CODES
            .trim()
            .lines()
            .enumerate()
            .for_each(|(high_bits, line)| {
                let codes_str: Vec<&str> = line.split_terminator('|').collect::<Vec<_>>();

                assert_eq!(17, codes_str.len());
                println!("\t// {:X?}x", high_bits);
                codes_str
                    .iter()
                    .skip(1)
                    .enumerate()
                    .for_each(|(low_bits, &code)| {
                        println!("\t{:?}, // x{:X?}", parse_opcode(code), low_bits)
                    });
            });
        println!("]");
    }

}
