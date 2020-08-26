use super::AddressingMode;
use super::Op;

// https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
// http://www.oxyron.de/html/opcodes02.html
#[derive(Debug, Clone, Copy)]
pub struct OpCode(pub Op, pub AddressingMode);

use AddressingMode::*;
use Op::*;
pub const OPCODES: [OpCode; 256] = [
    // 0x
    OpCode(BRK, Implicit),  // x0
    OpCode(ORA, IndirectX), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(SLO, IndirectX), // x3
    OpCode(NOP, ZeroPage),  // x4
    OpCode(ORA, ZeroPage),  // x5
    OpCode(ASL, ZeroPage),  // x6
    OpCode(SLO, ZeroPage),  // x7
    OpCode(PHP, Implicit),  // x8
    OpCode(ORA, Immediate), // x9
    OpCode(ASL, Implicit),  // xA
    OpCode(ANC, Immediate), // xB
    OpCode(NOP, Absolute),  // xC
    OpCode(ORA, Absolute),  // xD
    OpCode(ASL, Absolute),  // xE
    OpCode(SLO, Absolute),  // xF
    // 1x
    OpCode(BPL, Relative),  // x0
    OpCode(ORA, IndirectY), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(SLO, IndirectY), // x3
    OpCode(NOP, ZeroPageX), // x4
    OpCode(ORA, ZeroPageX), // x5
    OpCode(ASL, ZeroPageX), // x6
    OpCode(SLO, ZeroPageX), // x7
    OpCode(CLC, Implicit),  // x8
    OpCode(ORA, AbsoluteY), // x9
    OpCode(NOP, Implicit),  // xA
    OpCode(SLO, AbsoluteY), // xB
    OpCode(NOP, AbsoluteX), // xC
    OpCode(ORA, AbsoluteX), // xD
    OpCode(ASL, AbsoluteX), // xE
    OpCode(SLO, AbsoluteX), // xF
    // 2x
    OpCode(JSR, Absolute),  // x0
    OpCode(AND, IndirectX), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(RLA, IndirectX), // x3
    OpCode(BIT, ZeroPage),  // x4
    OpCode(AND, ZeroPage),  // x5
    OpCode(ROL, ZeroPage),  // x6
    OpCode(RLA, ZeroPage),  // x7
    OpCode(PLP, Implicit),  // x8
    OpCode(AND, Immediate), // x9
    OpCode(ROL, Implicit),  // xA
    OpCode(ANC, Immediate), // xB
    OpCode(BIT, Absolute),  // xC
    OpCode(AND, Absolute),  // xD
    OpCode(ROL, Absolute),  // xE
    OpCode(RLA, Absolute),  // xF
    // 3x
    OpCode(BMI, Relative),  // x0
    OpCode(AND, IndirectY), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(RLA, IndirectY), // x3
    OpCode(NOP, ZeroPageX), // x4
    OpCode(AND, ZeroPageX), // x5
    OpCode(ROL, ZeroPageX), // x6
    OpCode(RLA, ZeroPageX), // x7
    OpCode(SEC, Implicit),  // x8
    OpCode(AND, AbsoluteY), // x9
    OpCode(NOP, Implicit),  // xA
    OpCode(RLA, AbsoluteY), // xB
    OpCode(NOP, AbsoluteX), // xC
    OpCode(AND, AbsoluteX), // xD
    OpCode(ROL, AbsoluteX), // xE
    OpCode(RLA, AbsoluteX), // xF
    // 4x
    OpCode(RTI, Implicit),  // x0
    OpCode(EOR, IndirectX), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(SRE, IndirectX), // x3
    OpCode(NOP, ZeroPage),  // x4
    OpCode(EOR, ZeroPage),  // x5
    OpCode(LSR, ZeroPage),  // x6
    OpCode(SRE, ZeroPage),  // x7
    OpCode(PHA, Implicit),  // x8
    OpCode(EOR, Immediate), // x9
    OpCode(LSR, Implicit),  // xA
    OpCode(ALR, Immediate), // xB
    OpCode(JMP, Absolute),  // xC
    OpCode(EOR, Absolute),  // xD
    OpCode(LSR, Absolute),  // xE
    OpCode(SRE, Absolute),  // xF
    // 5x
    OpCode(BVC, Relative),  // x0
    OpCode(EOR, IndirectY), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(SRE, IndirectY), // x3
    OpCode(NOP, ZeroPageX), // x4
    OpCode(EOR, ZeroPageX), // x5
    OpCode(LSR, ZeroPageX), // x6
    OpCode(SRE, ZeroPageX), // x7
    OpCode(CLI, Implicit),  // x8
    OpCode(EOR, AbsoluteY), // x9
    OpCode(NOP, Implicit),  // xA
    OpCode(SRE, AbsoluteY), // xB
    OpCode(NOP, AbsoluteX), // xC
    OpCode(EOR, AbsoluteX), // xD
    OpCode(LSR, AbsoluteX), // xE
    OpCode(SRE, AbsoluteX), // xF
    // 6x
    OpCode(RTS, Implicit),  // x0
    OpCode(ADC, IndirectX), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(RRA, IndirectX), // x3
    OpCode(NOP, ZeroPage),  // x4
    OpCode(ADC, ZeroPage),  // x5
    OpCode(ROR, ZeroPage),  // x6
    OpCode(RRA, ZeroPage),  // x7
    OpCode(PLA, Implicit),  // x8
    OpCode(ADC, Immediate), // x9
    OpCode(ROR, Implicit),  // xA
    OpCode(ARR, Immediate), // xB
    OpCode(JMP, Indirect),  // xC
    OpCode(ADC, Absolute),  // xD
    OpCode(ROR, Absolute),  // xE
    OpCode(RRA, Absolute),  // xF
    // 7x
    OpCode(BVS, Relative),  // x0
    OpCode(ADC, IndirectY), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(RRA, IndirectY), // x3
    OpCode(NOP, ZeroPageX), // x4
    OpCode(ADC, ZeroPageX), // x5
    OpCode(ROR, ZeroPageX), // x6
    OpCode(RRA, ZeroPageX), // x7
    OpCode(SEI, Implicit),  // x8
    OpCode(ADC, AbsoluteY), // x9
    OpCode(NOP, Implicit),  // xA
    OpCode(RRA, AbsoluteY), // xB
    OpCode(NOP, AbsoluteX), // xC
    OpCode(ADC, AbsoluteX), // xD
    OpCode(ROR, AbsoluteX), // xE
    OpCode(RRA, AbsoluteX), // xF
    // 8x
    OpCode(NOP, Immediate), // x0
    OpCode(STA, IndirectX), // x1
    OpCode(NOP, Immediate), // x2
    OpCode(SAX, IndirectX), // x3
    OpCode(STY, ZeroPage),  // x4
    OpCode(STA, ZeroPage),  // x5
    OpCode(STX, ZeroPage),  // x6
    OpCode(SAX, ZeroPage),  // x7
    OpCode(DEY, Implicit),  // x8
    OpCode(NOP, Immediate), // x9
    OpCode(TXA, Implicit),  // xA
    OpCode(XAA, Immediate), // xB
    OpCode(STY, Absolute),  // xC
    OpCode(STA, Absolute),  // xD
    OpCode(STX, Absolute),  // xE
    OpCode(SAX, Absolute),  // xF
    // 9x
    OpCode(BCC, Relative),  // x0
    OpCode(STA, IndirectY), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(AHX, IndirectY), // x3
    OpCode(STY, ZeroPageX), // x4
    OpCode(STA, ZeroPageX), // x5
    OpCode(STX, ZeroPageY), // x6
    OpCode(SAX, ZeroPageY), // x7
    OpCode(TYA, Implicit),  // x8
    OpCode(STA, AbsoluteY), // x9
    OpCode(TXS, Implicit),  // xA
    OpCode(TAS, AbsoluteY), // xB
    OpCode(SHY, AbsoluteX), // xC
    OpCode(STA, AbsoluteX), // xD
    OpCode(SHX, AbsoluteY), // xE
    OpCode(AHX, AbsoluteY), // xF
    // Ax
    OpCode(LDY, Immediate), // x0
    OpCode(LDA, IndirectX), // x1
    OpCode(LDX, Immediate), // x2
    OpCode(LAX, IndirectX), // x3
    OpCode(LDY, ZeroPage),  // x4
    OpCode(LDA, ZeroPage),  // x5
    OpCode(LDX, ZeroPage),  // x6
    OpCode(LAX, ZeroPage),  // x7
    OpCode(TAY, Implicit),  // x8
    OpCode(LDA, Immediate), // x9
    OpCode(TAX, Implicit),  // xA
    OpCode(LAX, Immediate), // xB
    OpCode(LDY, Absolute),  // xC
    OpCode(LDA, Absolute),  // xD
    OpCode(LDX, Absolute),  // xE
    OpCode(LAX, Absolute),  // xF
    // Bx
    OpCode(BCS, Relative),  // x0
    OpCode(LDA, IndirectY), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(LAX, IndirectY), // x3
    OpCode(LDY, ZeroPageX), // x4
    OpCode(LDA, ZeroPageX), // x5
    OpCode(LDX, ZeroPageY), // x6
    OpCode(LAX, ZeroPageY), // x7
    OpCode(CLV, Implicit),  // x8
    OpCode(LDA, AbsoluteY), // x9
    OpCode(TSX, Implicit),  // xA
    OpCode(LAS, AbsoluteY), // xB
    OpCode(LDY, AbsoluteX), // xC
    OpCode(LDA, AbsoluteX), // xD
    OpCode(LDX, AbsoluteY), // xE
    OpCode(LAX, AbsoluteY), // xF
    // Cx
    OpCode(CPY, Immediate), // x0
    OpCode(CMP, IndirectX), // x1
    OpCode(NOP, Immediate), // x2
    OpCode(DCP, IndirectX), // x3
    OpCode(CPY, ZeroPage),  // x4
    OpCode(CMP, ZeroPage),  // x5
    OpCode(DEC, ZeroPage),  // x6
    OpCode(DCP, ZeroPage),  // x7
    OpCode(INY, Implicit),  // x8
    OpCode(CMP, Immediate), // x9
    OpCode(DEX, Implicit),  // xA
    OpCode(AXS, Immediate), // xB
    OpCode(CPY, Absolute),  // xC
    OpCode(CMP, Absolute),  // xD
    OpCode(DEC, Absolute),  // xE
    OpCode(DCP, Absolute),  // xF
    // Dx
    OpCode(BNE, Relative),  // x0
    OpCode(CMP, IndirectY), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(DCP, IndirectY), // x3
    OpCode(NOP, ZeroPageX), // x4
    OpCode(CMP, ZeroPageX), // x5
    OpCode(DEC, ZeroPageX), // x6
    OpCode(DCP, ZeroPageX), // x7
    OpCode(CLD, Implicit),  // x8
    OpCode(CMP, AbsoluteY), // x9
    OpCode(NOP, Implicit),  // xA
    OpCode(DCP, AbsoluteY), // xB
    OpCode(NOP, AbsoluteX), // xC
    OpCode(CMP, AbsoluteX), // xD
    OpCode(DEC, AbsoluteX), // xE
    OpCode(DCP, AbsoluteX), // xF
    // Ex
    OpCode(CPX, Immediate), // x0
    OpCode(SBC, IndirectX), // x1
    OpCode(NOP, Immediate), // x2
    OpCode(ISC, IndirectX), // x3
    OpCode(CPX, ZeroPage),  // x4
    OpCode(SBC, ZeroPage),  // x5
    OpCode(INC, ZeroPage),  // x6
    OpCode(ISC, ZeroPage),  // x7
    OpCode(INX, Implicit),  // x8
    OpCode(SBC, Immediate), // x9
    OpCode(NOP, Implicit),  // xA
    OpCode(SBC, Immediate), // xB
    OpCode(CPX, Absolute),  // xC
    OpCode(SBC, Absolute),  // xD
    OpCode(INC, Absolute),  // xE
    OpCode(ISC, Absolute),  // xF
    // Fx
    OpCode(BEQ, Relative),  // x0
    OpCode(SBC, IndirectY), // x1
    OpCode(KIL, Implicit),  // x2
    OpCode(ISC, IndirectY), // x3
    OpCode(NOP, ZeroPageX), // x4
    OpCode(SBC, ZeroPageX), // x5
    OpCode(INC, ZeroPageX), // x6
    OpCode(ISC, ZeroPageX), // x7
    OpCode(SED, Implicit),  // x8
    OpCode(SBC, AbsoluteY), // x9
    OpCode(NOP, Implicit),  // xA
    OpCode(ISC, AbsoluteY), // xB
    OpCode(NOP, AbsoluteX), // xC
    OpCode(SBC, AbsoluteX), // xD
    OpCode(INC, AbsoluteX), // xE
    OpCode(ISC, AbsoluteX), // xF
];
