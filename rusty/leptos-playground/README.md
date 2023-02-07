# README #

Play around the leptos. 

Use [leptos-rs/start](https://github.com/leptos-rs/start) as template.

for some reason, `cargo leptos watch` works but build & run don't.

`cargo leptos build --release` -> `cargo build --package=leptos_start --bin=leptos_start --target-dir=target/server --no-default-features --features=ssr --release`

`cargo leptos watch` -> `cargo build --package=leptos_start --lib --target-dir=target/front --target=wasm32-unknown-unknown --no-default-features --features=hydrate` 
&& 
`cargo build --package=leptos_start --bin=leptos_start --target-dir=target/server --no-default-features --features=ssr`


> env LEPTOS_OUTPUT_NAME="leptos_start" LEPTOS_SITE_ROOT="site" LEPTOS_SITE_PKG_DIR="pkg" LEPTOS_SITE_ADDR="127.0.0.1:3000" LEPTOS_RELOAD_PORT="3001" ./leptos_start
