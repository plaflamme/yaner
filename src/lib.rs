#![feature(generators, generator_trait)]

#[macro_use]
pub mod helper;
pub mod memory;
pub mod cartridge;
pub mod cpu;
pub mod ppu;
pub mod nes;

#[macro_use]
extern crate log;
