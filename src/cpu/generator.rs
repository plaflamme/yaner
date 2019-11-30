use super::*;
use Op::*;
use AddressingMode::*;
use std::fmt::Display;

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

#[derive(Debug)]
enum ModeOp {
    branch,

    read,
    modify,
    write,

    stack,

    unimplemented
}

struct Code(AddressingMode, ModeOp, Op);

impl Display for Code {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Code(_, ModeOp::unimplemented, _) => write!(f, "unimplemented!()"),
            Code(mode, _, Op::JMP) => write!(f, "{}jmp(self, mem_map)", mode),
            Code(AddressingMode::Implicit, ModeOp::read, op) => write!(f, "implicit::run(&{}, self, mem_map)", op),
            Code(_, ModeOp::stack, op) => {
                write!(f, "stack::{}(self, mem_map)", op)
            },
            Code(addr, mode, op) => {
                write!(f, "{}{:?}(&{}, self, mem_map)", addr, mode, op)
            }
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

        let mode = match op {
            ADC => ModeOp::read,
            AHX => ModeOp::unimplemented,
            ANC => ModeOp::unimplemented,
            AND => ModeOp::read,
            ALR => ModeOp::unimplemented,
            ARR => ModeOp::unimplemented,
            ASL => ModeOp::modify,
            AXS => ModeOp::unimplemented,

            BCC => ModeOp::branch,
            BCS => ModeOp::branch,
            BEQ => ModeOp::branch,

            BIT => ModeOp::read,

            BMI => ModeOp::branch,
            BNE => ModeOp::branch,
            BPL => ModeOp::branch,

            BRK => ModeOp::unimplemented,

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
            KIL => ModeOp::unimplemented,

            LAX => ModeOp::read,
            LAS => ModeOp::unimplemented,

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

            RRA => ModeOp::unimplemented,

            RTI => ModeOp::stack,
            RTS => ModeOp::stack,

            SAX => ModeOp::write,
            SBC => ModeOp::read,

            SEC => ModeOp::read,
            SED => ModeOp::read,
            SEI => ModeOp::read,

            SHX => ModeOp::unimplemented,
            SHY => ModeOp::unimplemented,
            SLO => ModeOp::modify,
            SRE => ModeOp::modify,

            STA => ModeOp::write,
            STX => ModeOp::write,
            STY => ModeOp::write,

            TAS => ModeOp::unimplemented,
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

    fn parse_opcode(code: &str) -> Code {
        let (code_str, _) = match code.len() {
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

        parse_code(code_str)
    }

    println!("{{");
    RAW_CODES
        .trim()
        .lines()
        .enumerate()
        .for_each(|(high_bits, line)| {
            let codes_str: Vec<&str> = line.split_terminator('|').collect::<Vec<_>>();
            assert_eq!(17, codes_str.len());
            codes_str
                .iter()
                .skip(1)
                .map(|&code_str|{
                    let code = parse_opcode(code_str);
                    match code {
                        Code(AddressingMode::Implicit, ModeOp::modify, op) => Code(AddressingMode::Accumulator, ModeOp::modify, op),
                        _ => code
                    }
                })
                .enumerate()
                .for_each(|(low_bits, kind)| {
                    let code = (high_bits as u8) << 4 | low_bits as u8;
                    match kind {
                        Code(_, ModeOp::unimplemented, _) => (),
                        _ => println!("\t0x{:02X?} => yield_complete!({}),", code, kind)
                    }
                });
        });
    println!("\t _ => {{ println!(\"{{:?}}\", instr); unimplemented!() }}");
    println!("}}");
}
