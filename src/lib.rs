#![feature(cell_update, generators, generator_trait, slice_fill)]

#[macro_use]
pub mod helper;
pub mod cartridge;
pub mod cpu;
#[allow(non_upper_case_globals)] // https://github.com/bitflags/bitflags/issues/198
pub mod input;
pub mod memory;
pub mod nes;
pub mod ppu;
pub mod retro;
pub mod tui;
