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
    );

    let result = nes.ram().read_u16(0x02);
    assert_eq!(0x00, result);
}

fn run_test(rom_path: &Path, start_at: Option<u16>) -> Nes {
    let cart = Cartridge::try_from(rom_path.to_owned()).unwrap();
    let nes = Nes::new(cart);
    nes.run(start_at);
    nes
}
