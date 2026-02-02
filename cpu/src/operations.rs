#![allow(non_camel_case_types)]
#![allow(dead_code)]
use super::*;

pub(super) trait BranchOperation {
    fn branch(&self, cpu: &Cpu) -> bool;
}

pub(super) trait ImplicitOperation {
    fn run(&self, cpu: &Cpu);
}

pub(super) trait ModifyOperation {
    // this includes addr because TAS, SHX and SHY operate on the high byte of the target address
    //   instead of the value
    fn modify(&self, cpu: &Cpu, addr: u16, value: u8) -> (u16, u8) {
        (addr, self.operate(cpu, value))
    }
    fn operate(&self, cpu: &Cpu, value: u8) -> u8;
}

pub(super) trait ReadOperation {
    fn operate(&self, cpu: &Cpu, value: u8);
}

pub(super) trait WriteOperation {
    fn operate(&self, cpu: &Cpu) -> u8;
}

// http://nesdev.com/6502_cpu.txt
// http://nesdev.com/undocumented_opcodes.txt
// http://www.oxyron.de/html/opcodes02.html

pub struct bcc;
impl BranchOperation for bcc {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.flag(Flags::C)
    }
}

pub struct bcs;
impl BranchOperation for bcs {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.flag(Flags::C)
    }
}

pub struct beq;
impl BranchOperation for beq {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.flag(Flags::Z)
    }
}

pub struct bmi;
impl BranchOperation for bmi {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.flag(Flags::N)
    }
}

pub struct bne;
impl BranchOperation for bne {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.flag(Flags::Z)
    }
}

pub struct bpl;
impl BranchOperation for bpl {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.flag(Flags::N)
    }
}

pub struct bvc;
impl BranchOperation for bvc {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.flag(Flags::V)
    }
}

pub struct bvs;
impl BranchOperation for bvs {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.flag(Flags::V)
    }
}

pub struct AdcResult {
    r: u8,
    c: bool,
    v: bool,
}
fn do_adc(a: u8, b: u8, c: u8) -> AdcResult {
    let (r, o1) = a.overflowing_add(b);
    let (r, o2) = r.overflowing_add(c);
    AdcResult {
        r,
        c: o1 || o2,
        v: (b ^ r) & (a ^ r) & 0x80 != 0,
    }
}

// http://obelisk.me.uk/6502/reference.html#ADC
pub struct adc;
impl ReadOperation for adc {
    fn operate(&self, cpu: &Cpu, v: u8) {
        let AdcResult { r, c, v } = do_adc(cpu.acc.get(), v, cpu.flag(Flags::C) as u8);
        cpu.set_flag(Flags::C, c);
        cpu.set_flag(Flags::V, v);

        cpu.acc.set(r);
        cpu.set_flags_from_acc();
    }
}

pub struct alr;
impl ReadOperation for alr {
    fn operate(&self, cpu: &Cpu, v: u8) {
        // ALR #{imm} = AND #{imm} + LSR
        and.operate(cpu, v);
        cpu.acc.set(lsr.operate(cpu, cpu.acc.get()));
    }
}

pub struct anc;
impl ReadOperation for anc {
    fn operate(&self, cpu: &Cpu, v: u8) {
        // ANC #{imm} = AND #{imm} + (ASL)
        and.operate(cpu, v);
        let result = cpu.acc.get();
        cpu.set_flag(Flags::C, (result & 0x80) != 0);
    }
}

pub struct and;
impl ReadOperation for and {
    fn operate(&self, cpu: &Cpu, v: u8) {
        cpu.acc.set(cpu.acc.get() & v);
        cpu.set_flags_from_acc();
    }
}

pub struct arr;
impl ReadOperation for arr {
    fn operate(&self, cpu: &Cpu, v: u8) {
        // ARR #{imm} = AND #{imm} + ROR (but not exactly)
        let result = ((cpu.acc.get() & v) >> 1) | (cpu.flag(Flags::C) as u8) << 7;

        let bit_6 = (result >> 6) & 1;
        let bit_5 = (result >> 5) & 1;
        cpu.set_flag(Flags::C, bit_6 == 1);
        cpu.set_flag(Flags::V, bit_6 ^ bit_5 == 1);
        cpu.acc.set(result);
        cpu.set_flags_from_acc();
    }
}

pub struct axs;
impl ReadOperation for axs {
    fn operate(&self, cpu: &Cpu, v: u8) {
        // AXS #{imm} = A&X minus #{imm} into X
        let a_x = cpu.acc.get() & cpu.x.get();
        let AdcResult { r, c, v: _ } = do_adc(a_x, !v, 1); // carry is always included
        cpu.x.set(r);
        cpu.set_flags_from(r);
        cpu.set_flag(Flags::C, c);
    }
}

pub struct bit;
impl ReadOperation for bit {
    fn operate(&self, cpu: &Cpu, v: u8) {
        let r = cpu.acc.get() & v;
        cpu.set_flag(Flags::Z, r == 0);
        cpu.set_flag(Flags::V, (v & 0x40) != 0); // set to the 6th bit of the value
        cpu.set_flag(Flags::N, (v & 0x80) != 0); // set to the 7th bit of the value
    }
}

pub struct eor;
impl ReadOperation for eor {
    fn operate(&self, cpu: &Cpu, v: u8) {
        cpu.acc.set(cpu.acc.get() ^ v);
        cpu.set_flags_from_acc();
    }
}

pub struct lax;
impl ReadOperation for lax {
    fn operate(&self, cpu: &Cpu, v: u8) {
        cpu.acc.set(v);
        cpu.x.set(v);
        cpu.set_flags_from_acc();
    }
}

pub struct las;
impl ReadOperation for las {
    fn operate(&self, cpu: &Cpu, v: u8) {
        let value = cpu.sp.get() & v;
        cpu.acc.set(value);
        cpu.x.set(value);
        cpu.sp.set(value);
        cpu.set_flags_from_acc();
    }
}

pub struct ora;
impl ReadOperation for ora {
    fn operate(&self, cpu: &Cpu, v: u8) {
        cpu.acc.set(cpu.acc.get() | v);
        cpu.set_flags_from_acc();
    }
}

pub struct sbc;
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

pub struct cmp;
impl ReadOperation for cmp {
    fn operate(&self, cpu: &Cpu, v: u8) {
        compare(cpu, cpu.acc.get(), v);
    }
}

pub struct cpx;
impl ReadOperation for cpx {
    fn operate(&self, cpu: &Cpu, v: u8) {
        compare(cpu, cpu.x.get(), v);
    }
}

pub struct cpy;
impl ReadOperation for cpy {
    fn operate(&self, cpu: &Cpu, v: u8) {
        compare(cpu, cpu.y.get(), v);
    }
}

// http://obelisk.me.uk/6502/reference.html#LDA
pub struct lda;
impl ReadOperation for lda {
    fn operate(&self, cpu: &Cpu, value: u8) {
        cpu.acc.set(value);
        cpu.set_flags_from_acc();
    }
}

// http://obelisk.me.uk/6502/reference.html#LDX
pub struct ldx;
impl ReadOperation for ldx {
    fn operate(&self, cpu: &Cpu, value: u8) {
        cpu.x.set(value);
        cpu.set_flags_from(value);
    }
}

// http://obelisk.me.uk/6502/reference.html#LDY
pub struct ldy;
impl ReadOperation for ldy {
    fn operate(&self, cpu: &Cpu, value: u8) {
        cpu.y.set(value);
        cpu.set_flags_from(value);
    }
}

pub struct xaa;
impl ReadOperation for xaa {
    fn operate(&self, cpu: &Cpu, value: u8) {
        // A:=X&#{imm}
        txa.run(cpu);
        and.operate(cpu, value);
    }
}

// http://obelisk.me.uk/6502/reference.html#ASL
pub struct asl;
impl ModifyOperation for asl {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = v << 1;
        cpu.set_flag(Flags::C, (v & 0x80) != 0);
        cpu.set_flags_from(result);
        result
    }
}

pub struct dcp;
impl ModifyOperation for dcp {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = dec.operate(cpu, v);
        cmp.operate(cpu, result);
        result
    }
}

pub struct dec;
impl ModifyOperation for dec {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = v.wrapping_sub(1);
        cpu.set_flags_from(result);
        result
    }
}

pub struct inc;
impl ModifyOperation for inc {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = v.wrapping_add(1);
        cpu.set_flags_from(result);
        result
    }
}

pub struct isc;
impl ModifyOperation for isc {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = inc.operate(cpu, v);
        sbc.operate(cpu, result);
        result
    }
}

// http://obelisk.me.uk/6502/reference.html#LSR
pub struct lsr;
impl ModifyOperation for lsr {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        cpu.set_flag(Flags::C, (v & 0x01) != 0);
        let result = v >> 1;
        cpu.set_flags_from(result);
        result
    }
}

pub struct rla;
impl ModifyOperation for rla {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = rol.operate(cpu, v);
        and.operate(cpu, result);
        result
    }
}

// http://obelisk.me.uk/6502/reference.html#ROL
pub struct rol;
impl ModifyOperation for rol {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = (v << 1) | cpu.flag(Flags::C) as u8;
        cpu.set_flag(Flags::C, v & 0x80 != 0);
        cpu.set_flags_from(result);
        result
    }
}

// http://obelisk.me.uk/6502/reference.html#ROR
pub struct ror;
impl ModifyOperation for ror {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = (v >> 1) | ((cpu.flag(Flags::C) as u8) << 7);
        cpu.set_flag(Flags::C, v & 0x01 != 0);
        cpu.set_flags_from(result);
        result
    }
}

pub struct rra;
impl ModifyOperation for rra {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = ror.operate(cpu, v);
        adc.operate(cpu, result);
        result
    }
}

//  http://forums.nesdev.com/viewtopic.php?f=3&t=10698&start=15
//  https://forums.nesdev.com/viewtopic.php?f=3&t=1fn 4063
//  https://github.com/starrhorne/nes-rust/blob/master/src/cpu.rs#L1153
//  http://www.ffd2.com/fridge/docs/6502-NMOS.extra.opcodes
pub struct shx;
impl ModifyOperation for shx {
    fn modify(&self, cpu: &Cpu, addr: u16, _: u8) -> (u16, u8) {
        let addr_orig = addr - (cpu.y.get() as u16);
        let oops = (addr_orig & 0xFF00) != (addr & 0xFF00);
        let addr_fixed = if !oops {
            addr
        } else {
            addr & ((cpu.x.get() as u16) << 8)
        };

        let value = cpu.x.get() & ((addr_fixed >> 8) as u8 + 1);
        (addr_fixed, value)
    }

    fn operate(&self, _: &Cpu, _: u8) -> u8 {
        unimplemented!()
    }
}

//  http://forums.nesdev.com/viewtopic.php?f=3&t=10698&start=15
//  https://forums.nesdev.com/viewtopic.php?f=3&t=1fn 4063
//  https://github.com/starrhorne/nes-rust/blob/master/src/cpu.rs#L1153
//  http://www.ffd2.com/fridge/docs/6502-NMOS.extra.opcodes
pub struct shy;
impl ModifyOperation for shy {
    fn modify(&self, cpu: &Cpu, addr: u16, _: u8) -> (u16, u8) {
        let addr_orig = addr - (cpu.x.get() as u16);
        let oops = (addr_orig & 0xFF00) != (addr & 0xFF00);
        let addr_fixed = if !oops {
            addr
        } else {
            addr & ((cpu.y.get() as u16) << 8)
        };

        let value = cpu.y.get() & ((addr_fixed >> 8) as u8 + 1);
        (addr_fixed, value)
    }

    fn operate(&self, _: &Cpu, _: u8) -> u8 {
        unimplemented!()
    }
}

pub struct slo;
impl ModifyOperation for slo {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = asl.operate(cpu, v);
        ora.operate(cpu, result);
        result
    }
}

pub struct sre;
impl ModifyOperation for sre {
    fn operate(&self, cpu: &Cpu, v: u8) -> u8 {
        let result = lsr.operate(cpu, v);
        eor.operate(cpu, result);
        result
    }
}

pub struct tas;
impl ModifyOperation for tas {
    fn modify(&self, cpu: &Cpu, addr: u16, _: u8) -> (u16, u8) {
        // TAS {adr} = stores A&X into S and A&X&H into {adr}
        let a_x = cpu.acc.get() & cpu.x.get();
        cpu.sp.set(a_x);
        let result = a_x & ((addr >> 8) as u8).wrapping_add(1);

        (addr, result)
    }

    fn operate(&self, _cpu: &Cpu, _value: u8) -> u8 {
        unimplemented!()
    }
}

pub struct sax;
impl WriteOperation for sax {
    fn operate(&self, cpu: &Cpu) -> u8 {
        cpu.acc.get() & cpu.x.get()
    }
}

// http://obelisk.me.uk/6502/reference.html#STA
pub struct sta;
impl WriteOperation for sta {
    fn operate(&self, cpu: &Cpu) -> u8 {
        cpu.acc.get()
    }
}

// http://obelisk.me.uk/6502/reference.html#STX
pub struct stx;
impl WriteOperation for stx {
    fn operate(&self, cpu: &Cpu) -> u8 {
        cpu.x.get()
    }
}

// http://obelisk.me.uk/6502/reference.html#STY
pub struct sty;
impl WriteOperation for sty {
    fn operate(&self, cpu: &Cpu) -> u8 {
        cpu.y.get()
    }
}

// http://obelisk.me.uk/6502/reference.html#CLC
pub struct clc;
impl ImplicitOperation for clc {
    fn run(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::C, false);
    }
}

// http://obelisk.me.uk/6502/reference.html#CLD
pub struct cld;
impl ImplicitOperation for cld {
    fn run(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::D, false);
    }
}

// http://obelisk.me.uk/6502/reference.html#CLI
pub struct cli;
impl ImplicitOperation for cli {
    fn run(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::I, false);
    }
}

// http://obelisk.me.uk/6502/reference.html#CLV
pub struct clv;
impl ImplicitOperation for clv {
    fn run(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::V, false);
    }
}

pub struct dex;
impl ImplicitOperation for dex {
    fn run(&self, cpu: &Cpu) {
        cpu.x.set(dec.operate(cpu, cpu.x.get()));
    }
}

pub struct dey;
impl ImplicitOperation for dey {
    fn run(&self, cpu: &Cpu) {
        cpu.y.set(dec.operate(cpu, cpu.y.get()));
    }
}

pub struct inx;
impl ImplicitOperation for inx {
    fn run(&self, cpu: &Cpu) {
        cpu.x.set(inc.operate(cpu, cpu.x.get()));
    }
}

pub struct iny;
impl ImplicitOperation for iny {
    fn run(&self, cpu: &Cpu) {
        cpu.y.set(inc.operate(cpu, cpu.y.get()));
    }
}

// http://obelisk.me.uk/6502/reference.html#NOP
pub struct nop;
impl ImplicitOperation for nop {
    fn run(&self, _: &Cpu) {}
}

// http://obelisk.me.uk/6502/reference.html#SEC
pub struct sec;
impl ImplicitOperation for sec {
    fn run(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::C, true);
    }
}

// http://obelisk.me.uk/6502/reference.html#SED
pub struct sed;
impl ImplicitOperation for sed {
    fn run(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::D, true);
    }
}

// http://obelisk.me.uk/6502/reference.html#SEI
pub struct sei;
impl ImplicitOperation for sei {
    fn run(&self, cpu: &Cpu) {
        cpu.set_flag(Flags::I, true);
    }
}

// http://obelisk.me.uk/6502/reference.html#TAY
pub struct tay;
impl ImplicitOperation for tay {
    fn run(&self, cpu: &Cpu) {
        cpu.y.set(cpu.acc.get());
        cpu.set_flags_from(cpu.y.get());
    }
}

// http://obelisk.me.uk/6502/reference.html#TAX
pub struct tax;
impl ImplicitOperation for tax {
    fn run(&self, cpu: &Cpu) {
        cpu.x.set(cpu.acc.get());
        cpu.set_flags_from(cpu.x.get());
    }
}

// http://obelisk.me.uk/6502/reference.html#TSX
pub struct tsx;
impl ImplicitOperation for tsx {
    fn run(&self, cpu: &Cpu) {
        let sp = cpu.sp.get();
        cpu.x.set(sp);
        cpu.set_flags_from(sp);
    }
}

// http://obelisk.me.uk/6502/reference.html#TXA
pub struct txa;
impl ImplicitOperation for txa {
    fn run(&self, cpu: &Cpu) {
        cpu.acc.set(cpu.x.get());
        cpu.set_flags_from_acc();
    }
}

// http://obelisk.me.uk/6502/reference.html#TXS
pub struct txs;
impl ImplicitOperation for txs {
    fn run(&self, cpu: &Cpu) {
        cpu.sp.set(cpu.x.get());
    }
}

// http://obelisk.me.uk/6502/reference.html#TYA
pub struct tya;
impl ImplicitOperation for tya {
    fn run(&self, cpu: &Cpu) {
        cpu.acc.set(cpu.y.get());
        cpu.set_flags_from_acc();
    }
}
