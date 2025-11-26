#![feature(coroutines, coroutine_trait)]

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use pprof::criterion::{Output, PProfProfiler};
use std::convert::TryFrom;
use std::path::Path;
use std::time::Duration;
use yaner::cartridge::Cartridge;
use yaner::nes::{Nes, Steps};

extern crate yaner;

fn run(stepper: &mut Steps, frames: u64) {
    for _ in 0..frames {
        stepper.step_frame().unwrap();
    }
}

fn ppu_benchmark(c: &mut Criterion) {
    let frames = 600;
    let mut group = c.benchmark_group("ppu-throughput");
    group.throughput(Throughput::Elements(frames));

    let cart = Cartridge::try_from(
        Path::new("roms/nes-test-roms/spritecans-2011/spritecans.nes").to_owned(),
    )
    .unwrap();
    let nes = Nes::new(cart);
    let mut stepper = nes.steps();

    group.measurement_time(Duration::from_secs(60));
    group.sample_size(10);
    group.bench_function(format!("{frames}"), |b| {
        b.iter(|| run(&mut stepper, frames))
    });
    group.finish();
}

criterion_group!(
    name = benches;
    // Run with `cargo bench -- --profile-time <time>`
    config = Criterion::default().with_profiler(PProfProfiler::new(1000, Output::Flamegraph(None)));
    targets = ppu_benchmark
);
criterion_main!(benches);
