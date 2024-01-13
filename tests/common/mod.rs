use std::convert::TryFrom;
use std::path::PathBuf;

use yaner::cartridge::Cartridge;
use yaner::nes::debug::NesState;
use yaner::nes::{Nes, Stepper};
use yaner::Reset;

pub mod blargg;

// result of evaluating the nes state between ppu frames
// used to determine what the test loop should do
pub enum Eval {
    Continue, // test should continue
    Reset,    // should issue a reset
    Halt,     // test should stop
}

pub fn run_test(
    rom_path: impl Into<PathBuf>,
    start_at: Option<u16>,
    mut eval: impl FnMut(&NesState) -> Eval,
    assert: impl FnOnce(&NesState),
) {
    let cart = Cartridge::try_from(rom_path.into()).unwrap();
    {
        let mut stepper = Stepper::new(Nes::new_with_pc(cart, start_at));
        loop {
            stepper.step_frame().unwrap();
            match eval(&stepper.nes().debug()) {
                Eval::Continue => (),
                Eval::Reset => stepper.reset(),
                Eval::Halt => break,
            }
        }
        assert(&stepper.nes().debug());
    }
}

// runs a test for a specific number of frames
pub fn run_test_frames(
    rom_path: impl Into<PathBuf>,
    start_at: Option<u16>,
    frames: u16,
    assert: impl FnOnce(&NesState),
) {
    let mut counter = 0;
    run_test(
        rom_path,
        start_at,
        |_| {
            counter += 1;
            if counter >= frames {
                return Eval::Halt;
            }
            Eval::Continue
        },
        assert,
    )
}
