#![feature(generators, generator_trait)]

mod common;

use common::blargg::run_blargg_test;
use common::run_test_frames;
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
    run_test_frames(
        "roms/nes-test-roms/cpu_timing_test6/cpu_timing_test.nes",
        None,
        640,
        |nes| {
            // NOTE: these were shown to be different when the test fails. It's not documented anywhere though...
            assert_eq!(nes.cpu_bus.read_u8(0x0000), 0x60);
            assert_eq!(nes.cpu_bus.read_u8(0x0001), 0xE6);
        },
    );
}

#[test]
#[should_panic] // requires APU
fn instr_timing() {
    run_blargg_test("roms/nes-test-roms/instr_timing/instr_timing.nes");
}

#[test]
#[ignore] // we pass this, but I'm not sure where to look to find the correct passing state.
fn cpu_dummy_reads() {
    run_blargg_test("roms/nes-test-roms/cpu_dummy_reads/cpu_dummy_reads.nes");
}

// we fail this due to writes to read-only memory. Not clear yet how this will be fixed.
#[test_case("cpu_dummy_writes_oam" => panics "tried writing to a read only location")]
#[test_case("cpu_dummy_writes_ppumem" => panics "tried writing to a read only location")]
fn cpu_dummy_writes(case: &str) {
    run_blargg_test(format!("roms/nes-test-roms/cpu_dummy_writes/{}.nes", case).as_str());
}

#[test_case("1.Branch_Basics")]
#[test_case("2.Backward_Branch")]
#[test_case("3.Forward_Branch")]
fn cpu_branch_timing_test(case: &str) {
    run_test_frames(
        format!("roms/nes-test-roms/branch_timing_tests/{}.nes", case),
        None,
        30,
        |nes| {
            // NOTE: these were shown to be different when the test fails. It's not documented anywhere though...
            assert_eq!(nes.cpu_bus.read_u8(0x0000), 0x80);
            assert_eq!(nes.cpu_bus.read_u8(0x0001), 0xE0);
        },
    );
}

#[test]
fn cpu_reset() {
    run_blargg_test("roms/nes-test-roms/cpu_reset/ram_after_reset.nes");
}

#[test_case("test_cpu_exec_space_ppuio")]
#[test_case("test_cpu_exec_space_apu" => panics "Failure To Obey Predetermined Execution Path")] // requires more io ports
fn cpu_exec_space(case: &str) {
    run_blargg_test(format!("roms/nes-test-roms/cpu_exec_space/{}.nes", case).as_str());
}

#[test_case("1-cli_latency" => panics "assertion `left == right` failed")]
#[test_case("2-nmi_and_brk" => panics "assertion `left == right` failed")]
#[test_case("3-nmi_and_irq" => panics "assertion `left == right` failed")]
#[test_case("4-irq_and_dma" => panics "assertion `left == right` failed")]
// #[test_case("5-branch_delays_irq" => panics "assertion failed")] // runs forever...
fn cpu_interrupts_v2(case: &str) {
    run_blargg_test(
        format!(
            "roms/nes-test-roms/cpu_interrupts_v2/rom_singles/{}.nes",
            case
        )
        .as_str(),
    );
}
