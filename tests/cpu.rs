#![feature(generators, generator_trait)]

mod common;

use common::blargg::run_blargg_test;
use test_case::test_case;

#[test_case("01-basics")]
#[test_case("02-implied")]
#[test_case("03-immediate")]
#[test_case("04-zero_page")]
#[test_case("05-zp_xy")]
#[test_case("06-absolute")]
#[test_case("07-abs_xy")]
#[test_case("08-ind_x")]
#[test_case("09-ind_y")]
#[test_case("10-branches")]
#[test_case("11-stack")]
#[test_case("12-jmp_jsr")]
#[test_case("13-rts")]
#[test_case("14-rti")]
#[test_case("15-brk")]
#[test_case("16-special")]
fn instr_test(case: &str) {
    run_blargg_test(format!("roms/nes-test-roms/instr_test-v5/rom_singles/{}.nes", case).as_str());
}

#[test_case("01-abs_x_wrap")]
#[test_case("02-branch_wrap")]
#[test_case("03-dummy_reads")]
#[test_case("04-dummy_reads_apu" => panics "not implemented")] // TODO
fn instr_misc(case: &str) {
    run_blargg_test(format!("roms/nes-test-roms/instr_misc/rom_singles/{}.nes", case).as_str());
}
