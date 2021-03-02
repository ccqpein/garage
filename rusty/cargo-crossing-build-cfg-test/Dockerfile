FROM alpine:latest
WORKDIR /app
COPY target/x86_64-unknown-linux-musl/debug/cargo-crossing-build-cfg-test /app/
ENTRYPOINT ["/app/cargo-crossing-build-cfg-test"]