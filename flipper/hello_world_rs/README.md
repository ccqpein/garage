# README #

## Steps ##

1. `rustup target add thumbv7em-none-eabihf`
2. `git clone git@github.com:dcoles/flipperzero-rs.git`
3. `cd flipperzero-rs/examples/hello-rust`
4. `${FLIPPERZERO_FIRMWARE}/scripts/storage.py mkdir /ext/apps/Misc` or `${FLIPPERZERO_FIRMWARE}/scripts/storage.py send target/thumbv7em-none-eabihf/release/hello-rust /ext/apps/Misc/hello-rust.fap`

## Tips ##

need make sure the api version match the crate
