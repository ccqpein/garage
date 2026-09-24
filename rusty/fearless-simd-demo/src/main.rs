use std::time::Instant;

use fearless_simd::{Level, Simd, dispatch, prelude::*};
use fearless_simd_macros::simd;

// ===========================================================================
// Example 0: Trivial Arithmetic (Integer Doubling)
// Demonstrates why SIMD can appear "slower" or the same speed on simple loops:
// Memory bandwidth saturation + LLVM auto-vectorization.
// ===========================================================================

/// What the function does:
///   Doubles each `u32` in a slice using a conventional scalar `for` loop.
///
/// What LLVM does:
///   In `--release` mode (`-O3`), LLVM detects that iterations are independent
///   and auto-vectorizes this loop using ARM NEON!
///   Furthermore, LLVM unrolls it 4x, generating paired 128-bit loads (`ldp q0, q1`)
///   and vector additions (`add.4s`), processing 16 integers (64 bytes) per loop pass.
///
/// What SIMD does here:
///   Because doubling is just 1 addition cycle, the loop is completely memory-bandwidth
///   bound (~80µs to read/write 4MB). Since LLVM's auto-vectorizer already uses NEON
///   under the hood with aggressive 4x unrolling, manual 1x SIMD cannot be faster.
fn double_u32(values: &mut [u32]) {
    for value in values {
        *value = *value * 2;
    }
}

/// What the function does:
///   A SIMD-generic function using the `#[simd]` attribute with an unused token `_: S`.
///
/// What LLVM does:
///   `#[simd]` compiles this function with target-specific CPU feature flags
///   (e.g., `target_feature = "+neon"`). Because the inner loop is scalar, LLVM
///   auto-vectorizes it identically to `double_u32`.
///
/// What SIMD / dispatch does:
///   When called via `dispatch!(level, simd => ...)`, `dispatch!` performs a runtime
///   `match` to detect CPU capabilities and invokes this function through a closure.
///   This runtime dispatch adds ~40–80ns of overhead. On tiny arrays (e.g. 5 elements),
///   this 50ns dispatch overhead dwarfs the 2ns calculation!
#[simd]
fn double_u32s<S: Simd>(_: S, values: &mut [u32]) {
    for value in values {
        *value = *value * 2;
    }
}

/// What the function does:
///   Explicitly vectorizes integer doubling using native SIMD vectors (`S::u32s`).
///   Processes 4 elements per iteration on 128-bit NEON, then handles leftovers.
///
/// What LLVM does:
///   - In `--release`: Inlines `from_slice`, `* 2`, and `store_slice` into direct
///     NEON instructions: `ldr q0, [addr]`, `add.4s v0, v0, v0`, and `str q0, [addr]`.
///   - In debug mode (`cargo run` without `--release`): Inlining is disabled, so
///     every chunk executes ~8 method calls and bounds checks, making it ~6x slower!
///
/// What SIMD does:
///   Guarantees hardware vector instructions across all platforms (NEON on ARM,
///   SSE2/AVX2 on x86, SIMD128 on WASM) without relying on compiler guesswork.
///   Because this loop processes 1 vector (16 bytes) per iteration, LLVM's 4x
///   unrolled auto-vectorizer (64 bytes/iter) had slightly lower branch overhead.
#[simd]
fn double_u32s_explicit<S: Simd>(simd: S, values: &mut [u32]) {
    let mut chunks = values.chunks_exact_mut(S::u32s::LEN);
    for chunk in &mut chunks {
        let v = S::u32s::from_slice(simd, chunk);
        (v * 2).store_slice(chunk);
    }
    for value in chunks.into_remainder() {
        *value = *value * 2;
    }
}

// ===========================================================================
// Complex Math Example 3: Mandelbrot Set Escape-Time Iteration
// Demonstrates where SIMD delivers HUGE speedups (~3.8x on Apple Silicon):
// Data-dependent branches & early exits where LLVM auto-vectorization fails.
// ===========================================================================

/// What the function does:
///   Computes the Mandelbrot fractal escape-time for each complex coordinate `c = cr + i*ci`.
///   Starting from `z = 0`, it computes `z = z^2 + c` up to `max_iters`:
///     z_r_new = z_r^2 - z_i^2 + c_r
///     z_i_new = 2 * z_r * z_i + c_i
///   If `|z|^2 = z_r^2 + z_i^2 > 4.0`, the point has escaped and the loop breaks early.
///
/// What LLVM does:
///   LLVM's auto-vectorizer CANNOT vectorize this loop!
///   Because each coordinate escapes at an unpredictable iteration (variable trip count
///   with an inner `break`), LLVM cannot prove uniform control flow. LLVM is forced to
///   emit purely scalar assembly: 1 float multiplication and 1 scalar branch at a time.
///   CPU branch predictors also suffer penalties from unpredictable escape conditions.
///
/// What SIMD does (why it's 3.8x faster):
///   See `mandelbrot_simd` below for the vector implementation.
fn mandelbrot_scalar(cr: &[f32], ci: &[f32], out: &mut [u32], max_iters: u32) {
    for i in 0..out.len() {
        let c_r = cr[i];
        let c_i = ci[i];
        let mut z_r = 0.0f32;
        let mut z_i = 0.0f32;
        let mut iter = 0;
        while iter < max_iters {
            let z_r2 = z_r * z_r;
            let z_i2 = z_i * z_i;
            if z_r2 + z_i2 > 4.0 {
                break;
            }
            z_i = 2.0 * z_r * z_i + c_i;
            z_r = z_r2 - z_i2 + c_r;
            iter += 1;
        }
        out[i] = iter;
    }
}

/// What the function does:
///   Explicitly computes Mandelbrot escape times for 4 complex coordinates simultaneously
///   using 128-bit vector registers (`S::f32s` and `S::u32s` on NEON).
///
/// What LLVM does:
///   In `--release`, LLVM compiles each vector operation directly to native NEON instructions:
///   - Vector multiplies & adds: `fmul.4s`, `fadd.4s`, `fsub.4s`
///   - Vector comparison: `fcmgt.4s`
///   - Vector selection: `bsl.16b` (Bitwise Select)
///   Because we explicitly structured the data parallelism, LLVM doesn't have to guess.
///
/// What SIMD does (how it achieves ~3.8x speedup):
///   1. **Parallel arithmetic**: All 4 coordinates execute `z = z^2 + c` in the exact same cycles.
///   2. **Branchless lane masking**: Instead of branching, `mag2.simd_le(four)` creates a vector mask.
///   3. **Branchless conditional select**: `active.select(iters + one, iters)` uses the hardware
///      bitmask to increment ONLY the lanes that haven't escaped yet—with ZERO branch mispredictions!
///   4. **Early exit for the whole chunk**: `if active.to_bitmask() == 0 { break; }` breaks only
///      when all 4 lanes have finished.
#[simd]
fn mandelbrot_simd<S: Simd>(simd: S, cr: &[f32], ci: &[f32], out: &mut [u32], max_iters: u32) {
    let len = S::f32s::LEN;
    let mut cr_chunks = cr.chunks_exact(len);
    let mut ci_chunks = ci.chunks_exact(len);
    let mut out_chunks = out.chunks_exact_mut(len);

    let four = S::f32s::splat(simd, 4.0);
    let zero_f = S::f32s::splat(simd, 0.0);
    let zero_u = S::u32s::splat(simd, 0);
    let one_u = S::u32s::splat(simd, 1);

    for ((c_r_c, c_i_c), out_c) in cr_chunks
        .by_ref()
        .zip(ci_chunks.by_ref())
        .zip(out_chunks.by_ref())
    {
        let c_r = S::f32s::from_slice(simd, c_r_c);
        let c_i = S::f32s::from_slice(simd, c_i_c);
        let mut z_r = zero_f;
        let mut z_i = zero_f;
        let mut iters = zero_u;

        for _ in 0..max_iters {
            let z_r2 = z_r * z_r;
            let z_i2 = z_i * z_i;
            let mag2 = z_r2 + z_i2;
            let active = mag2.simd_le(four);
            if active.to_bitmask() == 0 {
                break;
            }
            iters = active.select(iters + one_u, iters);
            z_i = z_r * z_i * 2.0 + c_i;
            z_r = z_r2 - z_i2 + c_r;
        }
        iters.store_slice(out_c);
    }

    let rem_cr = cr_chunks.remainder();
    let rem_ci = ci_chunks.remainder();
    let rem_out = out_chunks.into_remainder();
    for i in 0..rem_out.len() {
        let c_r = rem_cr[i];
        let c_i = rem_ci[i];
        let mut z_r = 0.0f32;
        let mut z_i = 0.0f32;
        let mut iter = 0;
        while iter < max_iters {
            let z_r2 = z_r * z_r;
            let z_i2 = z_i * z_i;
            if z_r2 + z_i2 > 4.0 {
                break;
            }
            z_i = 2.0 * z_r * z_i + c_i;
            z_r = z_r2 - z_i2 + c_r;
            iter += 1;
        }
        rem_out[i] = iter;
    }
}

// ===========================================================================
// Complex Math Example 1: Polynomial evaluation + Square Root
// Demonstrates straight-line float math on a single contiguous slice.
// ===========================================================================

/// What the function does:
///   Evaluates a polynomial with Horner's method, followed by abs and square root:
///     f(x) = sqrt(|x * (x * (x * 1.5 + 2.0) - 3.0) + 1.0|)
///   Runs in-place across 1,000,000 `f32` elements.
///
/// What LLVM does:
///   Because there are NO conditional branches, NO early breaks, and NO aliasing
///   (single slice), LLVM's auto-vectorizer easily vectorizes this loop in `--release`!
///   LLVM emits NEON vector instructions (`fmul.4s`, `fadd.4s`, `fabs.4s`, `fsqrt.4s`)
///   and unrolls 4x (16 floats per loop pass). That is why the "scalar" code runs so
///   fast (~200µs)—it is actually executing on the NEON vector ALU!
///
/// What SIMD does:
///   See `complex_math_simd` below.
fn complex_math_scalar(values: &mut [f32]) {
    for x in values {
        let v = *x;
        let p = v * (v * (v * 1.5 + 2.0) - 3.0) + 1.0;
        *x = p.abs().sqrt();
    }
}

/// What the function does:
///   Explicitly computes the polynomial + abs + sqrt across native SIMD vectors (`S::f32s`).
///   Processes 4 x f32 at a time on 128-bit NEON registers.
///
/// What LLVM does:
///   Inlines vector operator overloads (`Mul`, `Add`, `Sub`) and methods (`abs()`, `sqrt()`)
///   into direct NEON instructions:
///   - `fmul.4s`, `fadd.4s`, `fsub.4s` (vector fused float arithmetic)
///   - `fabs.4s` (clears the IEEE-754 sign bit for 4 floats at once)
///   - `fsqrt.4s` (calculates 4 hardware square roots simultaneously in 1 instruction)
///
/// What SIMD does:
///   Guarantees true vector operations without depending on whether LLVM's auto-vectorizer
///   heuristics trigger. Because both this and the scalar loop compile to NEON `fsqrt.4s`,
///   their speeds are virtually identical (~200µs vs ~240µs).
#[simd]
fn complex_math_simd<S: Simd>(simd: S, values: &mut [f32]) {
    let mut chunks = values.chunks_exact_mut(S::f32s::LEN);
    for chunk in &mut chunks {
        let v = S::f32s::from_slice(simd, chunk);
        let p = v * (v * (v * 1.5 + 2.0) - 3.0) + 1.0;
        let res = p.abs().sqrt();
        res.store_slice(chunk);
    }
    for x in chunks.into_remainder() {
        let v = *x;
        let p = v * (v * (v * 1.5 + 2.0) - 3.0) + 1.0;
        *x = p.abs().sqrt();
    }
}

// ===========================================================================
// Complex Math Example 2: 3D Euclidean Distances
// Demonstrates multi-slice vectorization across coordinate arrays (X, Y, Z).
// ===========================================================================

/// What the function does:
///   Computes Euclidean magnitude: dist = sqrt(x^2 + y^2 + z^2) for 1,000,000 3D points.
///
/// What LLVM does:
///   In `--release`, LLVM attempts to auto-vectorize the multi-slice loop.
///   It must verify that the mutable output slice `out` does not overlap in memory
///   with input slices `xs`, `ys`, or `zs` (pointer aliasing check). Once verified,
///   it emits NEON vector instructions (`fmul.4s`, `fadd.4s`, `fsqrt.4s`).
///
/// What SIMD does:
///   See `euclidean_distance_3d_simd` below.
fn euclidean_distance_3d_scalar(xs: &[f32], ys: &[f32], zs: &[f32], out: &mut [f32]) {
    for i in 0..out.len() {
        out[i] = (xs[i] * xs[i] + ys[i] * ys[i] + zs[i] * zs[i]).sqrt();
    }
}

/// What the function does:
///   Explicitly loads 4 X's, 4 Y's, and 4 Z's into separate 128-bit NEON registers,
///   computes `x^2 + y^2 + z^2`, takes the vector square root, and stores 4 distances.
///
/// What LLVM does:
///   Directly compiles the zipped chunk loop into sequential vector loads:
///   - `ldr q0, [x_ptr]` (4 floats from xs)
///   - `ldr q1, [y_ptr]` (4 floats from ys)
///   - `ldr q2, [z_ptr]` (4 floats from zs)
///   - `fmul.4s`, `fadd.4s` for dot products
///   - `fsqrt.4s` for 4 square roots at once
///   - `str q0, [out_ptr]` (store 4 distances)
///
/// What SIMD does:
///   Processes 4 points simultaneously per iteration. At 1,000,000 points, this function
///   reads 12 MB (3 x 4 MB) and writes 4 MB. Memory bus traffic dominates the ~600µs time.
#[simd]
fn euclidean_distance_3d_simd<S: Simd>(
    simd: S,
    xs: &[f32],
    ys: &[f32],
    zs: &[f32],
    out: &mut [f32],
) {
    let len = S::f32s::LEN;
    let mut x_chunks = xs.chunks_exact(len);
    let mut y_chunks = ys.chunks_exact(len);
    let mut z_chunks = zs.chunks_exact(len);
    let mut out_chunks = out.chunks_exact_mut(len);

    for (((x_c, y_c), z_c), out_c) in x_chunks
        .by_ref()
        .zip(y_chunks.by_ref())
        .zip(z_chunks.by_ref())
        .zip(out_chunks.by_ref())
    {
        let x = S::f32s::from_slice(simd, x_c);
        let y = S::f32s::from_slice(simd, y_c);
        let z = S::f32s::from_slice(simd, z_c);
        let dist = (x * x + y * y + z * z).sqrt();
        dist.store_slice(out_c);
    }

    let rem_x = x_chunks.remainder();
    let rem_y = y_chunks.remainder();
    let rem_z = z_chunks.remainder();
    let rem_out = out_chunks.into_remainder();
    for i in 0..rem_out.len() {
        rem_out[i] = (rem_x[i] * rem_x[i] + rem_y[i] * rem_y[i] + rem_z[i] * rem_z[i]).sqrt();
    }
}

fn main() {
    let mut values = [1, 2, 3, 4, 5];
    let level = Level::new();
    println!("Detected SIMD level: {:?}", level);
    let now = Instant::now();
    dispatch!(level, simd => double_u32s(simd, &mut values));
    println!(
        "first (dispatch autovec, 5 items): {:?}",
        Instant::now().duration_since(now)
    );
    assert_eq!(values, [2, 4, 6, 8, 10]);

    let mut values = [1, 2, 3, 4, 5];
    let now = Instant::now();
    double_u32(&mut values);
    println!(
        "second (plain scalar, 5 items):   {:?}",
        Instant::now().duration_since(now)
    );
    assert_eq!(values, [2, 4, 6, 8, 10]);

    // Benchmark with 1,000,000 items to show SIMD throughput:
    println!("\n--- Large slice (1,000,000 elements) ---");
    let mut large_scalar = vec![1u32; 1_000_000];
    let mut large_simd = vec![1u32; 1_000_000];

    let now = Instant::now();
    double_u32(&mut large_scalar);
    println!("scalar:        {:?}", Instant::now().duration_since(now));

    let now = Instant::now();
    dispatch!(level, simd => double_u32s_explicit(simd, &mut large_simd));
    println!("explicit SIMD: {:?}", Instant::now().duration_since(now));
    // Complex Math Example 1: Polynomial evaluation + Square Root
    println!("\n--- Complex Math 1: Polynomial + Sqrt (1,000,000 elements) ---");
    let mut math_scalar = vec![1.234f32; 1_000_000];
    let mut math_simd = vec![1.234f32; 1_000_000];

    let now = Instant::now();
    complex_math_scalar(&mut math_scalar);
    let dur_scalar = Instant::now().duration_since(now);
    println!("scalar:        {:?}", dur_scalar);

    let now = Instant::now();
    dispatch!(level, simd => complex_math_simd(simd, &mut math_simd));
    let dur_simd = Instant::now().duration_since(now);
    println!("explicit SIMD: {:?}", dur_simd);
    assert!((math_scalar[0] - math_simd[0]).abs() < 1e-5);

    // Complex Math Example 2: 3D Euclidean Distances
    println!("\n--- Complex Math 2: 3D Euclidean Distance (1,000,000 points) ---");
    let xs = vec![3.0f32; 1_000_000];
    let ys = vec![4.0f32; 1_000_000];
    let zs = vec![5.0f32; 1_000_000];
    let mut dists_scalar = vec![0.0f32; 1_000_000];
    let mut dists_simd = vec![0.0f32; 1_000_000];

    let now = Instant::now();
    euclidean_distance_3d_scalar(&xs, &ys, &zs, &mut dists_scalar);
    let dist_scalar_time = Instant::now().duration_since(now);
    std::hint::black_box(&dists_scalar);
    println!("scalar:        {:?}", dist_scalar_time);

    let now = Instant::now();
    dispatch!(level, simd => euclidean_distance_3d_simd(simd, &xs, &ys, &zs, &mut dists_simd));
    let dist_simd_time = Instant::now().duration_since(now);
    std::hint::black_box(&dists_simd);
    println!("explicit SIMD: {:?}", dist_simd_time);
    assert!((dists_scalar[0] - dists_simd[0]).abs() < 1e-5);
    // Complex Math Example 3: Mandelbrot Set (Escape-Time Iteration)
    // 50 iterations per point on 200,000 points
    println!("\n--- Complex Math 3: Mandelbrot Escape-Time (200,000 points) ---");
    let n = 200_000;
    let cr: Vec<f32> = (0..n).map(|i| -2.0 + (i as f32 / n as f32) * 2.5).collect();
    let ci: Vec<f32> = (0..n).map(|i| -1.2 + (i as f32 / n as f32) * 2.4).collect();
    let mut mandel_scalar = vec![0u32; n];
    let mut mandel_simd = vec![0u32; n];

    let now = Instant::now();
    mandelbrot_scalar(&cr, &ci, &mut mandel_scalar, 50);
    let mandel_scalar_time = Instant::now().duration_since(now);
    println!("scalar:        {:?}", mandel_scalar_time);

    let now = Instant::now();
    dispatch!(level, simd => mandelbrot_simd(simd, &cr, &ci, &mut mandel_simd, 50));
    let mandel_simd_time = Instant::now().duration_since(now);
    println!("explicit SIMD: {:?}", mandel_simd_time);
    println!(
        "Speedup:       {:.2}x",
        mandel_scalar_time.as_secs_f64() / mandel_simd_time.as_secs_f64()
    );
    assert_eq!(mandel_scalar, mandel_simd);
}
