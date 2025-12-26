use criterion::{Criterion, criterion_group, criterion_main};
use l_cache_test::*;
use std::hint::black_box;

fn sequential_access(c: &mut Criterion) {
    let mut group = c.benchmark_group("Memory Access");

    let mut data = create_data();
    group.bench_function("sequential_access_step_by_4", |b| {
        b.iter(|| sequential_access_step4(black_box(&mut data)))
    });

    let mut data = create_data();
    group.bench_function("sequential_access_step_by_16", |b| {
        b.iter(|| sequential_access_step16(black_box(&mut data)))
    });

    println!(
        "Benchmarking with detected L1 Cache Size: {} bytes",
        L1_CACHE_SIZE
    );

    println!(
        "Benchmarking with detected Cache Line Size: {} bytes",
        CACHE_LINE_SIZE
    );

    group.finish();
}

criterion_group!(memory_access_long_array_with_diff_steps, sequential_access);

criterion_main!(memory_access_long_array_with_diff_steps);
