#![feature(generators, generator_trait)]

use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use std::convert::TryFrom;
use std::path::Path;

use yaner::cartridge::Cartridge;
use yaner::nes::{Nes, Stepper};
use yaner::ppu::PpuCycle;

extern crate yaner;

fn run(stepper: &mut Stepper, frames: u64) {
    for _ in 0..frames {
        stepper.step_frame();
    }
}

fn ppu_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("ppu-throughput");
    for frames in vec![100, 200] {
        group.throughput(Throughput::Elements(frames));

        let cart = Cartridge::try_from(Path::new("roms/nes-test-roms/other/snow.nes").to_owned()).unwrap();
        let nes = Nes::new(cart);
        let mut stepper = Stepper::new(&nes, None);

        // group.sample_size(10);
        group.bench_function(format!("{}", frames), |b| {
            b.iter(||run(&mut stepper, frames))
        });
    }
    group.finish();
}

criterion_group!(benches, ppu_benchmark);
criterion_main!(benches);
