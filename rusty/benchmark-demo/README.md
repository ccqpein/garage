# README #

`cargo bench --bench bench0 -- --profile-time=10`

then check the `target/criterion`

+ [example](https://github.com/tikv/pprof-rs/blob/master/examples/criterion.rs)
+ [criterion](https://github.com/bheisler/criterion.rs)


## method 2 ##

use https://github.com/flamegraph-rs/flamegraph

`cargo install flamegraph`

`env CARGO_PROFILE_BENCH_DEBUG=true cargo flamegraph --bench bench0`

but I need to stop for SIP [blog](https://poweruser.blog/using-dtrace-with-sip-enabled-3826a352e64b):

```
Command-R in restarting

csrutil enable --without dtrace
```
