use criterion::{Criterion, black_box, criterion_group, criterion_main};
// Import functions from your library (assuming package name is 'cache_bench_demo')
use l_cache_test::{L1_CACHE_SIZE, create_data, fast_sequential_access, slow_strided_access};

fn benchmark_cache_access(c: &mut Criterion) {
    let data = create_data();

    let mut group = c.benchmark_group("Memory Access");

    // Test 1: Fast
    group.bench_function("sequential_access", |b| {
        b.iter(|| fast_sequential_access(black_box(&data)))
    });

    // Test 2: Slow (We purposefully access fewer elements, but jumping)
    // Note: Comparing strict time here is tricky because the stride function does
    // less total work (fewer additions).
    // To prove cache slowness strictly, we normally check "latency per access",
    // but for this demo, let's just run them to see the raw speed difference.
    group.bench_function("strided_access_misses", |b| {
        b.iter(|| slow_strided_access(black_box(&data)))
    });

    println!(
        "Benchmarking with detected L1 Cache Size: {} bytes",
        L1_CACHE_SIZE
    );

    group.finish();
}

criterion_group!(benches, benchmark_cache_access);
criterion_main!(benches);
