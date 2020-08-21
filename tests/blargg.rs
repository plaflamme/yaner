#![feature(generators, generator_trait)]

#[macro_use]
extern crate yaner;

use std::path::Path;

use yaner::memory::AddressSpace;

mod common;

#[test]
fn test_nes_instr_01() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/01-basics.nes"));
}

#[test]
fn test_nes_instr_02() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/02-implied.nes"));
}

#[test]
fn test_nes_instr_03() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/03-immediate.nes"));
}

#[test]
fn test_nes_instr_04() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/04-zero_page.nes"));
}

#[test]
fn test_nes_instr_05() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/05-zp_xy.nes"));
}

#[test]
fn test_nes_instr_06() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/06-absolute.nes"));
}

#[test]
fn test_nes_instr_07() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/07-abs_xy.nes"));
}

#[test]
fn test_nes_instr_08() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/08-ind_x.nes"));
}

#[test]
fn test_nes_instr_09() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/09-ind_y.nes"));
}

#[test]
fn test_nes_instr_10() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/10-branches.nes"));
}

#[test]
fn test_nes_instr_11() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/11-stack.nes"));
}

#[test]
fn test_nes_instr_12() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/12-jmp_jsr.nes"));
}

#[test]
fn test_nes_instr_13() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/13-rts.nes"));
}

#[test]
fn test_nes_instr_14() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/14-rti.nes"));
}

#[test]
fn test_nes_instr_15() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/15-brk.nes"));
}

#[test]
fn test_nes_instr_16() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_test-v5/rom_singles/16-special.nes"));
}

#[test]
fn test_instr_misc_01() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_misc/rom_singles/01-abs_x_wrap.nes"));
}

#[test]
fn test_instr_misc_02() {
    run_blargg_test(&Path::new("roms/nes-test-roms/instr_misc/rom_singles/02-branch_wrap.nes"));
}

#[test]
fn test_instr_misc_03() {
    // NOTE: This relies on the fact that reading PPUSTATUS resets the v-blank flag.
    //   It also requires precise timing between the CPU and PPU
    blargg_test(&Path::new("roms/nes-test-roms/instr_misc/rom_singles/03-dummy_reads.nes"), true);
}

#[test]
fn test_ppu_open_bus() {
    run_blargg_test(&Path::new("roms/nes-test-roms/ppu_open_bus/ppu_open_bus.nes"));
}

#[test]
fn test_oam_read() {
    run_blargg_test(&Path::new("roms/nes-test-roms/oam_read/oam_read.nes"));
}

#[test] #[ignore] // slow, run with cargo test -- --ignore
fn test_oam_stress() {
    run_blargg_test(&Path::new("roms/nes-test-roms/oam_stress/oam_stress.nes"));
}

fn read_zero_terminated_string(addr_space: &dyn AddressSpace, at: u16) -> String {
    let mut str_addr = at;
    let mut str_vec = Vec::new();
    let mut v = addr_space.read_u8(str_addr);
    while v != 0 {
        str_vec.push(v);
        str_addr += 1;
        v = addr_space.read_u8(str_addr);
    }
    String::from_utf8(str_vec).unwrap()
}

fn run_blargg_test(rom_path: &Path) {
    blargg_test(rom_path, true)
}

fn blargg_test(rom_path: &Path, expect_success: bool) {
    let mut result = 0xFF;
    let mut result_str = String::new();
    common::run_test(
        rom_path,
        None,
        |addr_space| {
            let marker = (addr_space.read_u8(0x6001), addr_space.read_u8(0x6002), addr_space.read_u8(0x6003));
            match marker {
                // this marker indicates that 0x6000 is useful
                (0xDE, 0xB0, 0x61) => {
                    match addr_space.read_u8(0x6000) {
                        0x80 => false, // test is running
                        done => {
                            result = done;
                            result_str = read_zero_terminated_string(addr_space, 0x6004);
                            true
                        }
                    }
                },
                _ => false
            }
        }
    );

    if expect_success {
        assert_eq!(0x00, result, "Test output: {}", result_str.trim());
    } else {
        assert_ne!(0x00, result, "Test output: {}", result_str.trim());
    }

}
