use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
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

fn cache_hit_with_increments(c: &mut Criterion) {
    // pin the core
    let core_ids = core_affinity::get_core_ids().unwrap();
    //println!("core_ids: {:?}", core_ids);
    let core_to_pin = core_ids[0]; // index 0 is P core 2025/12/26
    core_affinity::set_for_current(core_to_pin);

    let mut group = c.benchmark_group("Cache Line Hit");

    let increments = [1, 16, 512, 1024];

    for c in 1..=18 {
        for &step in increments.iter() {
            let parameter_string = format!("step:{} count:{}", step, c);
            group.bench_with_input(
                BenchmarkId::new("cache_hit_with_increments", parameter_string),
                &step,
                |b, &s| b.iter(|| cache_line_hit_with_increment(black_box(c), black_box(s))),
            );
        }
    }
    group.finish();
}

criterion_group!(memory_access_long_array_with_diff_steps, sequential_access);
criterion_group!(memory_access_with_increments, cache_hit_with_increments);

criterion_main!(
    memory_access_long_array_with_diff_steps,
    memory_access_with_increments
);
