#![feature(generators, generator_trait)]

mod common;

use common::blargg::run_blargg_test;
use test_case::test_case;

#[test]
fn ppu_open_bus() {
    run_blargg_test("roms/nes-test-roms/ppu_open_bus/ppu_open_bus.nes");
}

#[test]
fn oam_read() {
    run_blargg_test("roms/nes-test-roms/oam_read/oam_read.nes");
}

#[test]
#[ignore] // slow, run with cargo test -- --ignored
fn oam_stress() {
    run_blargg_test("roms/nes-test-roms/oam_stress/oam_stress.nes");
}

#[test_case("01-vbl_basics")]
#[test_case("02-vbl_set_time")]
#[test_case("03-vbl_clear_time")]
#[test_case("04-nmi_control")]
// #[test_case("05-nmi_timing")]
// #[test_case("06-suppression")]
// #[test_case("07-nmi_on_timing")]
// #[test_case("08-nmi_off_timing")]
// #[test_case("09-even_odd_frames")]
// #[test_case("10-even_odd_timing")]
fn ppu_vbl_nmi(case: &str) {
    run_blargg_test(format!("roms/nes-test-roms/ppu_vbl_nmi/rom_singles/{}.nes", case).as_str());
}
