#![feature(generators, generator_trait)]

#[macro_use]
extern crate yaner;

use std::path::Path;

mod common;

#[test]
fn test_nestest() {
    let nes = common::run_test(
        &Path::new("roms/nes-test-roms/other/nestest.nes"),
        Some(0xC000),
        |_| false
    );

    let result = nes.ram().read_u16(0x02);
    assert_eq!(0x00, result);
}
