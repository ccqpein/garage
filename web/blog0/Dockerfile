FROM rust:1-slim-bookworm AS builder

RUN apt-get update && apt-get install -y libssl-dev pkg-config
RUN rustup target add wasm32-unknown-unknown
RUN cargo install wasm-pack
RUN cargo install dioxus-cli

WORKDIR /app

# CMD ["dx", "bundle", "--platform", "web"]
