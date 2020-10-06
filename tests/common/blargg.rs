use super::run_test;

use std::path::Path;

use yaner::memory::AddressSpace;

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

pub fn run_blargg_test(rom_path: &str) {
    blargg_test(&Path::new(rom_path), true)
}

pub fn blargg_test(rom_path: &Path, expect_success: bool) {
    run_test(
        rom_path,
        None,
        |state| {
            let marker = (
                state.cpu_bus.read_u8(0x6001),
                state.cpu_bus.read_u8(0x6002),
                state.cpu_bus.read_u8(0x6003),
            );
            match marker {
                // this marker indicates that 0x6000 is useful
                (0xDE, 0xB0, 0x61) => {
                    match state.cpu_bus.read_u8(0x6000) {
                        0x80 => false, // test is running
                        _ => true
                    }
                }
                _ => false,
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
        }
    );

}
