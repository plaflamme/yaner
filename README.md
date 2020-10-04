# yaner

**Y**et **A**nother **N**ES **E**mulator in **R**ust.

![CI](https://github.com/plaflamme/yaner/workflows/CI/badge.svg)

My primary goal was to write a non-trivial program in Rust to learn more of it.
A secondary goal was to use its generators to implement the alternating cycles between the CPU and PPU.
A third goal was to get a cycle-accurate implementation.

Inspired from:

* [lochnes](https://github.com/kylewlacy/lochnes): for its use of Rust's generators, nice!
* [nes-rust](https://github.com/starrhorne/nes-rust): for the PPU implementation
* [laines](https://github.com/AndreaOrru/LaiNES): for more PPU stuff
* [Mesen](https://github.com/SourMesen/Mesen): for its amazing accuracy
