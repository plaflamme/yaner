#![feature(generators, generator_trait)]

mod common;

use common::blargg::run_blargg_test;
use common::run_test;
use std::path::Path;
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

#[test]
fn cpu_timing_test() {
    let mut frames = 0;
    run_test(
        &Path::new("roms/nes-test-roms/cpu_timing_test6/cpu_timing_test.nes"),
        None,
        |_| {
            frames += 1;
            frames >= 640
        },
        |nes| {
            // NOTE: these were shown to be different when the test fails. It's not documented anywhere though...
            assert_eq!(nes.cpu_bus.read_u8(0x0000), 0x60);
            assert_eq!(nes.cpu_bus.read_u8(0x0001), 0xE6);
        },
    );
}

#[test_case("1.Branch_Basics")]
#[test_case("2.Backward_Branch")]
#[test_case("3.Forward_Branch")]
fn cpu_branch_timing_test(case: &str) {
    let mut frames = 0;
    run_test(
        &Path::new(format!("roms/nes-test-roms/branch_timing_tests/{}.nes", case).as_str()),
        None,
        |_| {
            frames += 1;
            frames >= 30
        },
        |nes| {
            // NOTE: these were shown to be different when the test fails. It's not documented anywhere though...
            assert_eq!(nes.cpu_bus.read_u8(0x0000), 0x80);
            assert_eq!(nes.cpu_bus.read_u8(0x0001), 0xE0);
        },
    );
}
