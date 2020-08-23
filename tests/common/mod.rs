use std::convert::TryFrom;
use std::path::Path;

use yaner::cartridge::Cartridge;
use yaner::memory::AddressSpace;
use yaner::nes::Nes;

pub fn run_test(rom_path: &Path, start_at: Option<u16>, mut halt: impl FnMut(&dyn AddressSpace) -> bool) -> Nes {
    let cart = Cartridge::try_from(rom_path.to_owned()).unwrap();
    let nes = Nes::new(cart);
    consume_generator!(nes.ppu_frames(start_at), {
        if halt(nes.ram()) {
            break;
        }
    });
    nes
}
