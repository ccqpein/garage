use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use pprof::criterion::{Output, PProfProfiler};

fn format_macro(v: i32) -> String {
    format!("{}", v)
}

fn bench_format_macro(c: &mut Criterion) {
    let a = 1;
    c.bench_function("bench_format_macro", |b| {
        b.iter(|| format_macro(black_box(a.clone())))
    });
}

criterion_group! {
    name = bench_fault;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = bench_format_macro,
}

criterion_main!(bench_fault);
