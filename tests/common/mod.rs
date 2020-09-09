use std::convert::TryFrom;
use std::path::Path;

use yaner::cartridge::Cartridge;
use yaner::nes::{Nes, Stepper};
use yaner::nes::debug::NesState;

pub mod blargg;

pub fn run_test(
    rom_path: &Path,
    start_at: Option<u16>,
    mut halt: impl FnMut(&NesState) -> bool,
) -> Nes {
    let cart = Cartridge::try_from(rom_path.to_owned()).unwrap();
    let nes = Nes::new(cart);
    {
        let mut clock = Stepper::new(&nes, start_at);
        loop {
            clock.step_frame();
            if halt(&nes.debug()) {
                break;
            }
        }
    }
    nes
}
