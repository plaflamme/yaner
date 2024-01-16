#![feature(coroutines, coroutine_trait)]

mod common;

use common::blargg::{blargg_test, run_blargg_test};
use common::run_test_frames;
use test_case::test_case;

#[test]
fn ppu_open_bus() {
    run_blargg_test("roms/nes-test-roms/ppu_open_bus/ppu_open_bus.nes");
}

#[test]
#[should_panic] // needs APU? not sure.
fn ppu_read_buffer() {
    run_blargg_test("roms/nes-test-roms/ppu_read_buffer/test_ppu_read_buffer.nes");
}

#[test]
fn oam_read() {
    run_blargg_test("roms/nes-test-roms/oam_read/oam_read.nes");
}

#[test]
fn oam_stress() {
    run_blargg_test("roms/nes-test-roms/oam_stress/oam_stress.nes");
}

#[test_case("01-vbl_basics", true)]
#[test_case("02-vbl_set_time", true)]
#[test_case("03-vbl_clear_time", true)]
#[test_case("04-nmi_control", true)]
#[test_case("05-nmi_timing", false)]
#[test_case("06-suppression", false)]
#[test_case("07-nmi_on_timing", false)]
#[test_case("08-nmi_off_timing", false)]
#[test_case("09-even_odd_frames", true)]
#[test_case("10-even_odd_timing", false)]
fn ppu_vbl_nmi(case: &str, success: bool) {
    blargg_test(
        format!("roms/nes-test-roms/ppu_vbl_nmi/rom_singles/{}.nes", case),
        success,
    );
}

#[test_case("palette_ram")]
// #[test_case("power_up_palette")] Reports whether initial values in palette at power-up match those that my NES has. These values are probably unique to my NES.
#[test_case("sprite_ram")]
#[test_case("vbl_clear_time")]
#[test_case("vram_access")]
fn ppu_blargg_ppu_tests(case: &str) {
    run_test_frames(
        format!(
            "roms/nes-test-roms/blargg_ppu_tests_2005.09.15b/{}.nes",
            case
        ),
        None,
        30,
        |nes| assert_eq!(nes.ppu_bus.read_u8(0x20A4), 0x31),
    );
}

#[test_case("01.basics")]
#[test_case("02.alignment")]
#[test_case("03.corners")]
#[test_case("04.flip")]
#[test_case("05.left_clip")]
#[test_case("06.right_edge")]
#[test_case("07.screen_bottom")]
#[test_case("08.double_height")]
#[test_case("09.timing_basics")]
#[test_case("10.timing_order")]
#[test_case("11.edge_timing")]
fn ppu_sprite_hit_tests(case: &str) {
    run_test_frames(
        format!(
            "roms/nes-test-roms/sprite_hit_tests_2005.10.05/{}.nes",
            case
        ),
        None,
        80,
        |nes| {
            assert_eq!(nes.cpu_bus.read_u8(0x00F8), 0x01);
        },
    );
}

#[test_case("1.Basics")]
#[test_case("2.Details")]
#[test_case("3.Timing")] // http://forums.nesdev.com/viewtopic.php?t=2295
#[test_case("4.Obscure")]
#[test_case("5.Emulator")]
fn ppu_sprite_overflow_tests(case: &str) {
    run_test_frames(
        format!("roms/nes-test-roms/sprite_overflow_tests/{}.nes", case),
        None,
        160,
        |nes| {
            assert_eq!(nes.cpu_bus.read_u8(0x00F8), 0x01);
        },
    );
}
