#![feature(generators, generator_trait)]

use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};
use std::convert::TryFrom;
use std::path::Path;

use yaner::cartridge::Cartridge;
use yaner::nes::Nes;

#[macro_use]
extern crate yaner;

fn run(nes: &yaner::nes::Nes) {
    consume_generator!(nes.cpu.run(None), {
      if nes.ram().read_u8(0x6001) == 0xDE && nes.ram().read_u8(0x6002) == 0xB0 && nes.ram().read_u8(0x6003) == 0x61 {
        match nes.ram().read_u8(0x6000) {
          0x80 => (),
          _ => break
        }
      }
    });
}

fn criterion_benchmark(c: &mut Criterion) {

    let carts = vec![
        "01-basics.nes",
        "02-implied.nes",
        "03-immediate.nes",
        "04-zero_page.nes",
        "05-zp_xy.nes",
        "06-absolute.nes",
        "07-abs_xy.nes",
        "08-ind_x.nes",
        "09-ind_y.nes",
        "10-branches.nes",
        "11-stack.nes",
        "12-jmp_jsr.nes",
        "13-rts.nes",
        "14-rti.nes",
        "15-brk.nes",
        "16-special.nes",
    ];

    for &cart_name in carts.iter() {
        let cart = Cartridge::try_from(Path::new(&format!("roms/nes-test-roms/instr_test-v5/rom_singles/{}", cart_name)).to_owned()).unwrap();
        let nes = Nes::new(cart);
        c.bench_with_input(
            BenchmarkId::new("instr-test-v5", cart_name),
            &nes,
            |b, nes| b.iter(|| run(nes))
        );
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
