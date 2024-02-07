#![feature(coroutines, coroutine_trait)]
#![feature(type_alias_impl_trait)]
#[macro_use]
pub mod helper;
pub mod apu;
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
