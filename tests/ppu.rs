#![feature(generators, generator_trait)]

mod common;

use common::blargg::run_blargg_test;
use test_case::test_case;

#[test]
fn ppu_open_bus() {
    run_blargg_test(
        "roms/nes-test-roms/ppu_open_bus/ppu_open_bus.nes",
    );
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
