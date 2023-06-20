# README #

[tutorials link](https://www.sea-ql.org/SeaORM/docs/migration/setting-up-migration/)

## Usage ##

### Migration Table ###

~~`cargo install sea-orm-cli`~~ but looks like it has to use the `async-std`

### update 6/13/2023 ###

[some tutorial](https://dev.to/anshulxyz/guide-to-getting-started-with-seaorm-an-orm-for-rust-2fen)

try to use this project for sqlite

`sea-orm-cli migrate init`

**make sqlite db file**

`sqlite3 ./db/test_case.db`

then 

`.dump`

**migration**

`sea-orm-cli migrate init`

`cd migration && cargo run -- up`

**make the entity**

`cargo new entity --lib`

`sea-orm-cli generate entity -u sqlite://./db/test_case.db -o entity/src`

`cd entity && cargo add sea-orm && cd ..`

`mv entity/src/mod.rs entity/src/lib.rs` (I dont know why they still use mod)

