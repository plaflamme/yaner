use core::panic;
use quote::{ToTokens, TokenStreamExt, quote};
use std::{env, str::FromStr};
use strum_macros::EnumString;

#[derive(Debug, Clone, Copy, EnumString)]
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

impl ToTokens for Op {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let tt = match self {
            Op::ADC => quote!(operations::adc),
            Op::AND => quote!(operations::and),
            Op::ASL => quote!(operations::asl),
            Op::BCC => quote!(operations::bcc),
            Op::BCS => quote!(operations::bcs),
            Op::BEQ => quote!(operations::beq),
            Op::BIT => quote!(operations::bit),
            Op::BMI => quote!(operations::bmi),
            Op::BNE => quote!(operations::bne),
            Op::BPL => quote!(operations::bpl),
            Op::BRK => quote!(brk!()),
            Op::BVC => quote!(operations::bvc),
            Op::BVS => quote!(operations::bvs),
            Op::CLC => quote!(operations::clc),
            Op::CLD => quote!(operations::cld),
            Op::CLI => quote!(operations::cli),
            Op::CLV => quote!(operations::clv),
            Op::CMP => quote!(operations::cmp),
            Op::CPX => quote!(operations::cpx),
            Op::CPY => quote!(operations::cpy),
            Op::DEC => quote!(operations::dec),
            Op::DEX => quote!(operations::dex),
            Op::DEY => quote!(operations::dey),
            Op::EOR => quote!(operations::eor),
            Op::INC => quote!(operations::inc),
            Op::INX => quote!(operations::inx),
            Op::INY => quote!(operations::iny),
            Op::JMP => quote!(operations::jmp),
            Op::JSR => quote!(jsr!()),
            Op::LDA => quote!(operations::lda),
            Op::LDX => quote!(operations::ldx),
            Op::LDY => quote!(operations::ldy),
            Op::LSR => quote!(operations::lsr),
            Op::NOP => quote!(operations::nop),
            Op::ORA => quote!(operations::ora),
            Op::PHA => quote!(pha!()),
            Op::PHP => quote!(php!()),
            Op::PLA => quote!(pla!()),
            Op::PLP => quote!(plp!()),
            Op::ROL => quote!(operations::rol),
            Op::ROR => quote!(operations::ror),
            Op::RTI => quote!(rti!()),
            Op::RTS => quote!(rts!()),
            Op::SEC => quote!(operations::sec),
            Op::SED => quote!(operations::sed),
            Op::SEI => quote!(operations::sei),
            Op::STA => quote!(operations::sta),
            Op::STX => quote!(operations::stx),
            Op::STY => quote!(operations::sty),
            Op::TAX => quote!(operations::tax),
            Op::TAY => quote!(operations::tay),
            Op::TSX => quote!(operations::tsx),
            Op::TXA => quote!(operations::txa),
            Op::TXS => quote!(operations::txs),
            Op::TYA => quote!(operations::tya),
            Op::AHX => quote!(operations::ahx),
            Op::ALR => quote!(operations::alr),
            Op::ANC => quote!(operations::anc),
            Op::ARR => quote!(operations::arr),
            Op::AXS => quote!(operations::axs),
            Op::DCP => quote!(operations::dcp),
            Op::ISC => quote!(operations::isc),
            Op::LAS => quote!(operations::las),
            Op::LAX => quote!(operations::lax),
            Op::RLA => quote!(operations::rla),
            Op::RRA => quote!(operations::rra),
            Op::SAX => quote!(operations::sax),
            Op::SBC => quote!(operations::sbc),
            Op::SHX => quote!(operations::shx),
            Op::SHY => quote!(operations::shy),
            Op::SLO => quote!(operations::slo),
            Op::SRE => quote!(operations::sre),
            Op::TAS => quote!(operations::tas),
            Op::XAA => quote!(operations::xaa),
            Op::KIL => quote!(operations::kil),
        };
        tokens.append_all(tt);
    }
}

#[derive(Debug, Clone, Copy, Default, EnumString)]
pub enum AddressingMode {
    #[default]
    Imp, // Implied from the op, things like RTS or BRK
    Acc, // Accumulator, like implied, but the A register is what's implied
    #[strum(serialize = "imm")]
    Imm,
    #[strum(serialize = "zp")]
    Zp0,
    #[strum(serialize = "zpx")]
    ZpX,
    #[strum(serialize = "zpy")]
    ZpY,
    #[strum(serialize = "rel")]
    Rel,
    #[strum(serialize = "abs")]
    Abs,
    #[strum(serialize = "abx")]
    AbX,
    #[strum(serialize = "aby")]
    AbY,
    #[strum(serialize = "ind")]
    Ind,
    #[strum(serialize = "izx")]
    IdX,
    #[strum(serialize = "izy")]
    IdY,
}

#[derive(Debug, Clone, Copy)]
struct OpCode {
    op: Op,
    mode: AddressingMode,
}

impl ToTokens for OpCode {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let op_type = OpType::from_op(self.op);
        let op = self.op;

        if matches!(op, Op::KIL) {
            let tt = quote!(kil!());
            tokens.append_all(tt);
            return;
        }
        let tt = match self.mode {
            AddressingMode::Acc => quote!(acc!(#op)),
            AddressingMode::Abs => quote!(abs!(#op_type, #op)),
            AddressingMode::AbX => quote!(abs_indexed!(#op_type, #op, self.x.get())),
            AddressingMode::AbY => quote!(abs_indexed!(#op_type, #op, self.y.get())),
            AddressingMode::Zp0 => quote!(zp!(#op_type, #op)),
            AddressingMode::ZpX => quote!(zp_indexed!(#op_type, #op, self.x.get())),
            AddressingMode::ZpY => quote!(zp_indexed!(#op_type, #op, self.y.get())),
            AddressingMode::Imp => {
                if matches!(op_type, OpType::Stack) {
                    quote!(#op)
                } else {
                    quote!(imp!(#op))
                }
            }
            AddressingMode::Imm => quote!(imm!(#op_type, #op)),
            AddressingMode::Rel => quote!(rel!(#op_type, #op)),
            AddressingMode::Ind => quote!(ind!(#op_type, #op)),
            AddressingMode::IdX => quote!(ind_indexed_x!(#op_type, #op)),
            AddressingMode::IdY => quote!(ind_indexed_y!(#op_type, #op)),
        };
        tokens.append_all(tt);
    }
}

enum OpType {
    Read,
    Modify,
    Write,
    Branch,
    Stack,
    Implicit,
}

impl OpType {
    fn from_op(op: Op) -> OpType {
        use Op::*;
        match op {
            ADC => OpType::Read,

            AHX => OpType::Modify,

            ANC => OpType::Read,
            AND => OpType::Read,
            ALR => OpType::Read,
            ARR => OpType::Read,
            ASL => OpType::Modify,
            AXS => OpType::Read,

            BCC => OpType::Branch,
            BCS => OpType::Branch,
            BEQ => OpType::Branch,

            BIT => OpType::Read,

            BMI => OpType::Branch,
            BNE => OpType::Branch,
            BPL => OpType::Branch,

            BRK => OpType::Stack,

            BVC => OpType::Branch,
            BVS => OpType::Branch,

            CLC => OpType::Read,
            CLD => OpType::Read,
            CLI => OpType::Read,
            CLV => OpType::Read,

            CMP => OpType::Read,
            CPX => OpType::Read,
            CPY => OpType::Read,

            DCP => OpType::Modify,
            DEC => OpType::Modify,
            DEX => OpType::Read,
            DEY => OpType::Read,
            EOR => OpType::Read,
            INC => OpType::Modify,
            INX => OpType::Read,
            INY => OpType::Read,
            ISC => OpType::Modify,

            JMP => OpType::Implicit,

            JSR => OpType::Stack,
            KIL => OpType::Read,

            LAX => OpType::Read,
            LAS => OpType::Read,

            LDA => OpType::Read,
            LDX => OpType::Read,
            LDY => OpType::Read,

            LSR => OpType::Modify,

            NOP => OpType::Read,

            ORA => OpType::Read,

            PHA => OpType::Stack,
            PHP => OpType::Stack,
            PLA => OpType::Stack,
            PLP => OpType::Stack,

            RLA => OpType::Modify,

            ROL => OpType::Modify,
            ROR => OpType::Modify,

            RRA => OpType::Modify,

            RTI => OpType::Stack,
            RTS => OpType::Stack,

            SAX => OpType::Write,
            SBC => OpType::Read,

            SEC => OpType::Read,
            SED => OpType::Read,
            SEI => OpType::Read,

            SHX => OpType::Modify,
            SHY => OpType::Modify,

            SLO => OpType::Modify,
            SRE => OpType::Modify,

            STA => OpType::Write,
            STX => OpType::Write,
            STY => OpType::Write,

            TAS => OpType::Modify,
            TAX => OpType::Read,
            TAY => OpType::Read,

            TSX => OpType::Read,
            TXA => OpType::Read,
            TXS => OpType::Read,

            TYA => OpType::Read,
            XAA => OpType::Read, // TODO: not implemented
        }
    }
}

impl ToTokens for OpType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let tt = match self {
            OpType::Read => quote!(OpType::Read),
            OpType::Modify => quote!(OpType::Modify),
            OpType::Write => quote!(OpType::Write),
            OpType::Branch => quote!(OpType::Branch),
            OpType::Stack => quote!(OpType::Stack),
            OpType::Implicit => quote!(OpType::Implicit),
        };
        tokens.append_all(tt);
    }
}

fn parse_opcode(opcode: &str) -> Result<OpCode, Box<dyn std::error::Error>> {
    let parts = opcode.split_ascii_whitespace().collect::<Vec<_>>();
    let (op_str, addr_str) = match parts[..] {
        [code] => (code, None),
        [code, addr_or_timing] => (code, Some(addr_or_timing)),
        [code, addr, _] => (code, Some(addr)),
        _ => panic!("unexpeced input {}", opcode),
    };

    let op = Op::from_str(op_str)?;
    let mode = addr_str.and_then(|s| AddressingMode::from_str(s).ok());

    Ok(OpCode {
        op,
        mode: match mode {
            Some(mode) => mode,
            None => match op {
                Op::ASL | Op::LSR | Op::ROL | Op::ROR => AddressingMode::Acc,
                _ => AddressingMode::Imp,
            },
        },
    })
}

pub fn main() {
    let table = std::fs::read_to_string("src/opcode_table.csv").expect("read opcode table");
    let arms = table.lines().flat_map(|line| {
        let mut cols = line.split(",");
        let row_hex = cols.next().unwrap();
        let row = u8::from_str_radix(row_hex, 16).unwrap();
        cols.enumerate()
            .map(|(col, op)| {
                let opcode = row + col as u8;
                (
                    opcode,
                    parse_opcode(op.trim()).expect("should be able to parse all ops"),
                )
            })
            .map(|(opcode_value, opcode)| {
                let opcode_value = proc_macro2::Literal::from_str(&format!("0x{opcode_value:X}u8"))
                    .expect("hex literal works");
                quote! {
                    #opcode_value => { #opcode; }
                }
            })
            .collect::<Vec<_>>()
    });

    let pmatch = quote! {
        {
            match opcode {
                #(#arms)*
            };
        }
    };
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = std::path::Path::new(&out_dir).join("out.rs");
    std::fs::write(dest_path, format!("{pmatch}")).unwrap();
}
