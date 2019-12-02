use std::convert::TryFrom;
use std::path::Path;

use crate::cartridge::Cartridge;
use crate::nes::Nes;
use crate::memory::AddressSpace;

#[test]
fn test_nestest() {
    let nes = run_test(
        &Path::new("roms/nes-test-roms/other/nestest.nes"),
        Some(0xC000),
        |_| false
    );

    let result = nes.ram().read_u16(0x02);
    assert_eq!(0x00, result);
}

#[test]
fn test_nes_instr_01() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/01-implied.nes"));
}

#[test]
fn test_nes_instr_02() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/02-immediate.nes"));
}

#[test]
fn test_nes_instr_03() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/03-zero_page.nes"));
}

#[test]
fn test_nes_instr_04() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/04-zp_xy.nes"));
}

#[test]
fn test_nes_instr_05() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/05-absolute.nes"));
}

#[test]
fn test_nes_instr_06() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/06-abs_xy.nes"));
}

#[test]
fn test_nes_instr_07() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/07-ind_x.nes"));
}

#[test]
fn test_nes_instr_08() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/08-ind_y.nes"));
}

#[test]
fn test_nes_instr_09() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/09-branches.nes"));
}

#[test]
fn test_nes_instr_10() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/10-stack.nes"));
}

#[test]
fn test_nes_instr_11() {
    run_blargg_test(&Path::new("roms/nes-test-roms/nes_instr_test/rom_singles/11-special.nes"));
}

#[test]
fn test_ppu_open_bus() {
    run_blargg_test(&Path::new("roms/nes-test-roms/ppu_open_bus/ppu_open_bus.nes"));
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
    let mut started = false;
    let mut result = 0xFF;
    let mut result_str = String::new();
    run_test(
        rom_path,
        None,
        |addr_space| {
            match (addr_space.read_u8(0x6000), started) {
                (0x80, _) => {
                    started = true;
                    false
                },
                (other, true) => {
                    result = other;
                    result_str = read_zero_terminated_string(addr_space, 0x6004);
                    true
                },
                _ => false
            }
        }
    );

    println!("{}", result_str);
    assert_eq!(0x00, result);
}

fn run_test(rom_path: &Path, start_at: Option<u16>, halt: impl FnMut(&dyn AddressSpace) -> bool) -> Nes {
    let cart = Cartridge::try_from(rom_path.to_owned()).unwrap();
    let nes = Nes::new(cart);
    nes.run(start_at, halt);
    nes
}
