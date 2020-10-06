use std::convert::TryFrom;
use std::path::Path;

use yaner::cartridge::Cartridge;
use yaner::nes::debug::NesState;
use yaner::nes::{Nes, Stepper};

pub mod blargg;

pub fn run_test(
    rom_path: &Path,
    start_at: Option<u16>,
    mut halt: impl FnMut(&NesState) -> bool,
    mut assert: impl FnMut(&NesState),
) {
    let cart = Cartridge::try_from(rom_path.to_owned()).unwrap();
    {
        let mut stepper = Stepper::new(Nes::new(cart), start_at);
        loop {
            stepper.step_frame().unwrap();
            if halt(&stepper.nes().debug()) {
                break;
            }
        }
        assert(&stepper.nes().debug());
    }
}
