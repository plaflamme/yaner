#![feature(cell_update, coroutines, coroutine_trait)]
#![feature(type_alias_impl_trait)]
#![feature(as_array_of_cells)]
#[macro_use]
pub mod helper;
pub mod cartridge;
pub mod cpu;
pub mod input;
pub mod memory;
pub mod nes;
pub mod ppu;
pub mod retro;
pub mod tui;

// implement for things that reset
// not very useful at this point
pub trait Reset {
    fn reset(&self);
}
