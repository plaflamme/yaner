use super::run_test;

use std::path::PathBuf;

use crate::common::Eval;
use yaner::memory::AddressSpace;

pub fn read_zero_terminated_string(addr_space: &dyn AddressSpace, at: u16) -> String {
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

pub fn run_blargg_test(rom_path: impl Into<PathBuf>) {
    blargg_test(rom_path, true)
}

pub fn blargg_test(rom_path: impl Into<PathBuf>, expect_success: bool) {
    let mut reset_pending = None;
    run_test(
        rom_path,
        None,
        |state| {
            if let Some(remaining) = reset_pending {
                if remaining == 0 {
                    reset_pending = None;
                    return Eval::Reset;
                }
                reset_pending = Some(remaining - 1);
            }
            let marker = (
                state.cpu_bus.read_u8(0x6001),
                state.cpu_bus.read_u8(0x6002),
                state.cpu_bus.read_u8(0x6003),
            );
            match marker {
                // this marker indicates that 0x6000 is useful
                (0xDE, 0xB0, 0x61) => {
                    match state.cpu_bus.read_u8(0x6000) {
                        0x80 => Eval::Continue, // test is running
                        0x81 if reset_pending.is_some() => Eval::Continue,
                        0x81 => {
                            // requires reset, at least 100ms from now
                            reset_pending = Some(10);
                            Eval::Continue
                        }
                        _ => Eval::Halt,
                    }
                }
                _ => Eval::Continue,
            }
        },
        |state| {
            let result = state.cpu_bus.read_u8(0x6000);
            let result_str = read_zero_terminated_string(state.cpu_bus, 0x6004);

            if expect_success {
                assert_eq!(0x00, result, "Test output: {}", result_str.trim());
            } else {
                assert_ne!(0x00, result, "Test output: {}", result_str.trim());
            }
        },
    );
}
