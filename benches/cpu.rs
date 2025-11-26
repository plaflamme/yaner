#![feature(coroutines, coroutine_trait)]

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use pprof::criterion::{Output, PProfProfiler};
use std::ops::{Coroutine, CoroutineState};
use std::time::Duration;
use yaner_cpu::{Cpu, CpuEvent, CpuTick, Phi, Rw};

fn run(ram: &mut [u8], cycles: u64) {
    const PRG_START: u16 = 0x400;
    let cpu = Cpu::new(Some(PRG_START));
    let mut cpu_routine = cpu.run();
    for _ in 0..cycles {
        let cpu_routine = std::pin::Pin::new(&mut cpu_routine);
        match cpu_routine.resume(()) {
            CoroutineState::Yielded(CpuEvent::Tick(CpuTick {
                phi: Phi::Start,
                rw: Rw::Read,
                addr,
            })) => cpu.io_bus.set(ram[addr as usize]),
            CoroutineState::Yielded(CpuEvent::Tick(CpuTick {
                phi: Phi::Start,
                rw: Rw::Write,
                addr,
            })) => ram[addr as usize] = cpu.io_bus.get(),
            CoroutineState::Complete(_) => panic!("cpu stopped"),
            _ => (),
        }
    }
}

fn cpu_benchmark(c: &mut Criterion) {
    let cycles = 1_000_000;
    let mut group = c.benchmark_group("cpu-throughput");
    group.throughput(Throughput::Elements(cycles));

    let pgr = std::fs::read("roms/6502_65C02_functional_tests/bin_files/6502_functional_test.bin")
        .expect("rom is present");

    group.measurement_time(Duration::from_secs(60));
    group.bench_with_input(format!("{cycles}"), &pgr, |b, pgr| {
        let mut ram = [0; 0x10000];
        ram[0x0000..pgr.len()].copy_from_slice(pgr);
        b.iter(|| run(&mut ram, cycles));
    });
    group.finish();
}

criterion_group!(
    name = benches;
    // Run with `cargo bench -- --profile-time <time>`
    config = Criterion::default().with_profiler(PProfProfiler::new(1000, Output::Flamegraph(None)));
    targets = cpu_benchmark
);
criterion_main!(benches);
