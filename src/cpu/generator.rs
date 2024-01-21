use super::*;
use std::fmt::Display;
use AddressingMode::*;
use Op::*;

// http://www.oxyron.de/html/opcodes02.html
const RAW_CODES: &str = r#"
0x,BRK 7,ORA izx 6,KIL,SLO izx 8,NOP zp 3,ORA zp 3,ASL zp 5,SLO zp 5,PHP 3,ORA imm 2,ASL 2,ANC imm 2,NOP abs 4,ORA abs 4,ASL abs 6,SLO abs 6
1x,BPL rel 2*,ORA izy 5*,KIL,SLO izy 8,NOP zpx 4,ORA zpx 4,ASL zpx 6,SLO zpx 6,CLC 2,ORA aby 4*,NOP 2,SLO aby 7,NOP abx 4*,ORA abx 4*,ASL abx 7,SLO abx 7
2x,JSR abs 6,AND izx 6,KIL,RLA izx 8,BIT zp 3,AND zp 3,ROL zp 5,RLA zp 5,PLP 4,AND imm 2,ROL 2,ANC imm 2,BIT abs 4,AND abs 4,ROL abs 6,RLA abs 6
3x,BMI rel 2*,AND izy 5*,KIL,RLA izy 8,NOP zpx 4,AND zpx 4,ROL zpx 6,RLA zpx 6,SEC 2,AND aby 4*,NOP 2,RLA aby 7,NOP abx 4*,AND abx 4*,ROL abx 7,RLA abx 7
4x,RTI 6,EOR izx 6,KIL,SRE izx 8,NOP zp 3,EOR zp 3,LSR zp 5,SRE zp 5,PHA 3,EOR imm 2,LSR 2,ALR imm 2,JMP abs 3,EOR abs 4,LSR abs 6,SRE abs 6
5x,BVC rel 2*,EOR izy 5*,KIL,SRE izy 8,NOP zpx 4,EOR zpx 4,LSR zpx 6,SRE zpx 6,CLI 2,EOR aby 4*,NOP 2,SRE aby 7,NOP abx 4*,EOR abx 4*,LSR abx 7,SRE abx 7
6x,RTS 6,ADC izx 6,KIL,RRA izx 8,NOP zp 3,ADC zp 3,ROR zp 5,RRA zp 5,PLA 4,ADC imm 2,ROR 2,ARR imm 2,JMP ind 5,ADC abs 4,ROR abs 6,RRA abs 6
7x,BVS rel 2*,ADC izy 5*,KIL,RRA izy 8,NOP zpx 4,ADC zpx 4,ROR zpx 6,RRA zpx 6,SEI 2,ADC aby 4*,NOP 2,RRA aby 7,NOP abx 4*,ADC abx 4*,ROR abx 7,RRA abx 7
8x,NOP imm 2,STA izx 6,NOP imm 2,SAX izx 6,STY zp 3,STA zp 3,STX zp 3,SAX zp 3,DEY 2,NOP imm 2,TXA 2,XAA imm 2,STY abs 4,STA abs 4,STX abs 4,SAX abs 4
9x,BCC rel 2*,STA izy 6,KIL,AHX izy 6,STY zpx 4,STA zpx 4,STX zpy 4,SAX zpy 4,TYA 2,STA aby 5,TXS 2,TAS aby 5,SHY abx 5,STA abx 5,SHX aby 5,AHX aby 5
Ax,LDY imm 2,LDA izx 6,LDX imm 2,LAX izx 6,LDY zp 3,LDA zp 3,LDX zp 3,LAX zp 3,TAY 2,LDA imm 2,TAX 2,LAX imm 2,LDY abs 4,LDA abs 4,LDX abs 4,LAX abs 4
Bx,BCS rel 2*,LDA izy 5*,KIL,LAX izy 5*,LDY zpx 4,LDA zpx 4,LDX zpy 4,LAX zpy 4,CLV 2,LDA aby 4*,TSX 2,LAS aby 4*,LDY abx 4*,LDA abx 4*,LDX aby 4*,LAX aby 4*
Cx,CPY imm 2,CMP izx 6,NOP imm 2,DCP izx 8,CPY zp 3,CMP zp 3,DEC zp 5,DCP zp 5,INY 2,CMP imm 2,DEX 2,AXS imm 2,CPY abs 4,CMP abs 4,DEC abs 6,DCP abs 6
Dx,BNE rel 2*,CMP izy 5*,KIL,DCP izy 8,NOP zpx 4,CMP zpx 4,DEC zpx 6,DCP zpx 6,CLD 2,CMP aby 4*,NOP 2,DCP aby 7,NOP abx 4*,CMP abx 4*,DEC abx 7,DCP abx 7
Ex,CPX imm 2,SBC izx 6,NOP imm 2,ISC izx 8,CPX zp 3,SBC zp 3,INC zp 5,ISC zp 5,INX 2,SBC imm 2,NOP 2,SBC imm 2,CPX abs 4,SBC abs 4,INC abs 6,ISC abs 6
Fx,BEQ rel 2*,SBC izy 5*,KIL,ISC izy 8,NOP zpx 4,SBC zpx 4,INC zpx 6,ISC zpx 6,SED 2,SBC aby 4*,NOP 2,ISC aby 7,NOP abx 4*,SBC abx 4*,INC abx 7,ISC abx 7
"#;

#[derive(Debug)]
enum ModeOp {
    branch,

    read,
    modify,
    write,

    stack,

    unimplemented,
}

struct Code(AddressingMode, ModeOp, Op);

impl Display for Code {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Code(_, ModeOp::unimplemented, _) => write!(f, "unimplemented!()"),
            Code(AddressingMode::Implicit, ModeOp::read, op) => {
                write!(f, "implicit::run(&{}, self)", op)
            }
            Code(mode, _, Op::JMP) => write!(f, "{mode}jmp(self)"),
            Code(_, ModeOp::stack, op) => write!(f, "stack::{op}(self)"),
            Code(addr, mode, op) => write!(f, "{addr}{mode:?}(&{op}, self)"),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let fmt = format!("{:?}", self);
        write!(f, "{}", fmt.to_lowercase())
    }
}

impl Display for AddressingMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            AddressingMode::Absolute => write!(f, "absolute::"),
            AddressingMode::AbsoluteX => write!(f, "absolute_indexed::x_"),
            AddressingMode::AbsoluteY => write!(f, "absolute_indexed::y_"),

            AddressingMode::Immediate => write!(f, "immediate::"),

            AddressingMode::ZeroPage => write!(f, "zero_page::"),
            AddressingMode::ZeroPageX => write!(f, "zero_page_indexed::x_"),
            AddressingMode::ZeroPageY => write!(f, "zero_page_indexed::y_"),

            AddressingMode::Indirect => write!(f, "indirect::"),
            AddressingMode::IndirectX => write!(f, "indirect_indexed::x_"),
            AddressingMode::IndirectY => write!(f, "indirect_indexed::y_"),

            AddressingMode::Accumulator => write!(f, "accumulator::"),
            AddressingMode::Relative => write!(f, "relative::"),
            AddressingMode::Implicit => write!(f, "implicit::"),
        }
    }
}

pub fn generate_opcode_table() {
    fn parse_code(code: &str) -> Code {
        let parts = code.split_ascii_whitespace().collect::<Vec<_>>();
        let (op_str, addr_str) = match parts[..] {
            [code] => (code, None),
            [code, addr_or_timing] => (code, Some(addr_or_timing)),
            [code, addr, _] => (code, Some(addr)),
            _ => panic!("unexpeced input {}", code),
        };
        let op = match op_str {
            "ADC" => ADC,
            "AHX" => AHX,
            "ANC" => ANC,
            "AND" => AND,
            "ALR" => ALR,
            "ARR" => ARR,
            "ASL" => ASL,
            "AXS" => AXS,
            "BCC" => BCC,
            "BCS" => BCS,
            "BEQ" => BEQ,
            "BIT" => BIT,
            "BMI" => BMI,
            "BNE" => BNE,
            "BPL" => BPL,
            "BRK" => BRK,
            "BVC" => BVC,
            "BVS" => BVS,
            "CLC" => CLC,
            "CLD" => CLD,
            "CLI" => CLI,
            "CLV" => CLV,
            "CMP" => CMP,
            "CPX" => CPX,
            "CPY" => CPY,
            "DCP" => DCP,
            "DEC" => DEC,
            "DEX" => DEX,
            "DEY" => DEY,
            "EOR" => EOR,
            "INC" => INC,
            "INX" => INX,
            "INY" => INY,
            "ISC" => ISC,
            "JMP" => JMP,
            "JSR" => JSR,
            "KIL" => KIL,
            "LAS" => LAS,
            "LAX" => LAX,
            "LDA" => LDA,
            "LDX" => LDX,
            "LDY" => LDY,
            "LSR" => LSR,
            "NOP" => NOP,
            "ORA" => ORA,
            "PHA" => PHA,
            "PHP" => PHP,
            "PLA" => PLA,
            "PLP" => PLP,
            "RLA" => RLA,
            "ROL" => ROL,
            "ROR" => ROR,
            "RRA" => RRA,
            "RTI" => RTI,
            "RTS" => RTS,
            "SAX" => SAX,
            "SBC" => SBC,
            "SHX" => SHX,
            "SHY" => SHY,
            "SEC" => SEC,
            "SED" => SED,
            "SEI" => SEI,
            "SLO" => SLO,
            "SRE" => SRE,
            "STA" => STA,
            "STX" => STX,
            "STY" => STY,
            "TAS" => TAS,
            "TAX" => TAX,
            "TAY" => TAY,
            "TSX" => TSX,
            "TXA" => TXA,
            "TXS" => TXS,
            "TYA" => TYA,
            "XAA" => XAA,
            c => panic!("Unexpected code {}", c),
        };

        let addr = if let Some(str) = addr_str {
            match str {
                "zp" => ZeroPage,
                "zpx" => ZeroPageX,
                "zpy" => ZeroPageY,
                "ind" => Indirect,
                "izx" => IndirectX,
                "izy" => IndirectY,
                "abs" => Absolute,
                "abx" => AbsoluteX,
                "aby" => AbsoluteY,
                "imm" => Immediate,
                "rel" => Relative,
                _ => Implicit,
            }
        } else {
            Implicit
        };

        let mode = match op {
            ADC => ModeOp::read,

            AHX => ModeOp::modify,

            ANC => ModeOp::read,
            AND => ModeOp::read,
            ALR => ModeOp::read,
            ARR => ModeOp::read,
            ASL => ModeOp::modify,
            AXS => ModeOp::read,

            BCC => ModeOp::branch,
            BCS => ModeOp::branch,
            BEQ => ModeOp::branch,

            BIT => ModeOp::read,

            BMI => ModeOp::branch,
            BNE => ModeOp::branch,
            BPL => ModeOp::branch,

            BRK => ModeOp::stack,

            BVC => ModeOp::branch,
            BVS => ModeOp::branch,

            CLC => ModeOp::read,
            CLD => ModeOp::read,
            CLI => ModeOp::read,
            CLV => ModeOp::read,

            CMP => ModeOp::read,
            CPX => ModeOp::read,
            CPY => ModeOp::read,

            DCP => ModeOp::modify,
            DEC => ModeOp::modify,
            DEX => ModeOp::read,
            DEY => ModeOp::read,
            EOR => ModeOp::read,
            INC => ModeOp::modify,
            INX => ModeOp::read,
            INY => ModeOp::read,
            ISC => ModeOp::modify,

            JMP => ModeOp::read,

            JSR => ModeOp::stack,
            KIL => ModeOp::read,

            LAX => ModeOp::read,
            LAS => ModeOp::read,

            LDA => ModeOp::read,
            LDX => ModeOp::read,
            LDY => ModeOp::read,

            LSR => ModeOp::modify,

            NOP => ModeOp::read,

            ORA => ModeOp::read,

            PHA => ModeOp::stack,
            PHP => ModeOp::stack,
            PLA => ModeOp::stack,
            PLP => ModeOp::stack,

            RLA => ModeOp::modify,

            ROL => ModeOp::modify,
            ROR => ModeOp::modify,

            RRA => ModeOp::modify,

            RTI => ModeOp::stack,
            RTS => ModeOp::stack,

            SAX => ModeOp::write,
            SBC => ModeOp::read,

            SEC => ModeOp::read,
            SED => ModeOp::read,
            SEI => ModeOp::read,

            SHX => ModeOp::modify,
            SHY => ModeOp::modify,

            SLO => ModeOp::modify,
            SRE => ModeOp::modify,

            STA => ModeOp::write,
            STX => ModeOp::write,
            STY => ModeOp::write,

            TAS => ModeOp::modify,
            TAX => ModeOp::read,
            TAY => ModeOp::read,

            TSX => ModeOp::read,
            TXA => ModeOp::read,
            TXS => ModeOp::read,

            TYA => ModeOp::read,
            XAA => ModeOp::unimplemented,
        };

        Code(addr, mode, op)
    }

    println!("{{");
    RAW_CODES
        .trim()
        .lines()
        .enumerate()
        .for_each(|(high_bits, line)| {
            let codes_str: Vec<&str> = line.split_terminator(',').collect::<Vec<_>>();
            assert_eq!(17, codes_str.len());
            codes_str
                .iter()
                .skip(1)
                .map(|&code_str| {
                    let code = parse_code(code_str);
                    match code {
                        Code(AddressingMode::Implicit, ModeOp::modify, op) => {
                            Code(AddressingMode::Accumulator, ModeOp::modify, op)
                        }
                        _ => code,
                    }
                })
                .enumerate()
                .for_each(|(low_bits, kind)| {
                    let code = (high_bits as u8) << 4 | low_bits as u8;
                    match kind {
                        Code(_, _, Op::KIL) => {
                            println!("\t0x{:02X?} => OpTrace::Implicit,", code)
                        }
                        Code(_, ModeOp::unimplemented, _) => (),
                        _ => println!("\t0x{:02X?} => yield_complete!({}),", code, kind),
                    }
                });
        });
    println!("\t _ => {{ println!(\"{{:?}}\", &OPCODES[opcode as usize]); unimplemented!() }}");
    println!("}}");
}
