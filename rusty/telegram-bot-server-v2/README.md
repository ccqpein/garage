# README #

## Build ##

```shell
cat ~/.cargo/config.toml
[target.x86_64-unknown-linux-musl]
linker = "/opt/homebrew/bin/x86_64-linux-musl-gcc"
```

`env RUST_BACKTRACE=1 CC_x86_64_unknown_linux_musl=x86_64-linux-musl-gcc cargo build --target=x86_64-unknown-linux-musl --release`
