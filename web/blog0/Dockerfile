FROM rust:1-slim-bookworm AS builder

RUN apt-get update && apt-get install -y libssl-dev pkg-config musl-tools
RUN rustup target add wasm32-unknown-unknown
RUN rustup target add x86_64-unknown-linux-musl
RUN cargo install wasm-pack
RUN cargo install dioxus-cli

WORKDIR /app

# CMD ["dx", "bundle", "--platform", "web"]
