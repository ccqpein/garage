# README

my Apple M2 Max

E core:

>Efficiency (E) Cores:
>L1 Instruction Cache (L1i): 128 KB per core.
>L1 Data Cache (L1d): 64 KB per core.

P core:

>Performance (P) Cores:
>L1 Instruction Cache (L1i): 192 KB per core, 12-way set-associative.
>L1 Data Cache (L1d): 128 KB per core, 8-way set-associative.

128 KB = 131,072 B
131,072 B / 8 = 16,384 B per way = 16 KB

## Results

`cargo bench`

**memory_access_long_array_with_diff_steps**

```
Memory Access/sequential_access_step_by_4
                        time:   [5.2634 ms 5.2797 ms 5.2990 ms]
                        change: [−18.391% −13.075% −7.7682%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 5 outliers among 100 measurements (5.00%)
  3 (3.00%) high mild
  2 (2.00%) high severe
Memory Access/sequential_access_step_by_16
                        time:   [4.4792 ms 4.4846 ms 4.4915 ms]
                        change: [−41.474% −37.557% −33.196%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 6 outliers among 100 measurements (6.00%)
  5 (5.00%) high mild
  1 (1.00%) high severe
```

**memory_access_with_increments**

1024 * i32 (4 bytes) = 4096 bytes = 4kb

check 9 * 1024 (1024 * 4 * 9= 36kb) and 17 * 512 (512 * 4 * 17 = 34 KB)

| count\step |     1     |    16     |    512    |   1024    |
|:----------:|:---------:|:---------:|:---------:|:---------:|
|     1      | 14.623 ns | 20.203 ns | 39.497 ns | 60.158 ns |
|     2      | 14.959 ns | 16.719 ns | 61.666 ns | 117.56 ns |
|     3      | 15.316 ns | 16.682 ns | 86.723 ns | 155.49 ns |
|     4      | 15.523 ns | 17.043 ns | 116.55 ns | 191.88 ns |
|     5      | 18.830 ns | 26.392 ns | 129.64 ns | 233.49 ns |
|     6      | 19.095 ns | 27.074 ns | 156.91 ns | 294.41 ns |
|     7      | 19.339 ns | 27.871 ns | 172.78 ns | 329.69 ns |
|     8      | 19.754 ns | 28.192 ns | 190.90 ns | 432.96 ns |
|     9      | 20.547 ns | 28.936 ns | 240.19 ns | **1327.3 ns** |
|     10     | 20.838 ns | 29.175 ns | 249.96 ns | 1330.2 ns |
|     11     | 21.312 ns | 30.231 ns | 280.66 ns | 1402.7 ns |
|     12     | 21.545 ns | 30.510 ns | 283.95 ns | 1402.9 ns |
|     13     | 20.448 ns | 31.647 ns | 331.33 ns | 1466.5 ns |
|     14     | 20.798 ns | 31.434 ns | 324.00 ns | 1487.9 ns |
|     15     | 21.119 ns | 30.458 ns | 456.47 ns | 1474.7 ns |
|     16     | 21.363 ns | 30.754 ns | 397.97 ns | 1471.9 ns |
|     17     | 21.654 ns | 37.488 ns | **1322.7 ns** | 1589.9 ns |
|     18     | 21.879 ns | 37.473 ns | 1503.8 ns | 1709.9 ns |


**matrix_hit**

```
Matrix Hit/matrix_iter_row => 194.06 µs
Matrix Hit/matrix_iter_col => 437.18 µs
```
