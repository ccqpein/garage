FROM rust:1-slim-bookworm AS builder

RUN apt-get update && apt-get install -y libssl-dev pkg-config musl-tools
RUN rustup target add wasm32-unknown-unknown
RUN rustup target add x86_64-unknown-linux-musl
RUN cargo install wasm-pack
RUN cargo install dioxus-cli

WORKDIR /app

COPY . .

RUN dx bundle --platform web

FROM debian:bookworm-slim

WORKDIR /app

ENV PORT=8080
ENV IP=0.0.0.0

COPY --from=builder /app/target/dx/blog0/release/web/public ./public
COPY --from=builder /app/target/dx/blog0/release/web/server .

EXPOSE 8080

ENTRYPOINT ["./server"]
