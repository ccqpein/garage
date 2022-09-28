# README #

the demo project for playing around [diesel](https://diesel.rs/guides/getting-started)

## TIPS ##

The `schema.rs` changed when the sql files in migrations changed. don't need to change by myself

need install `libpq`

`pg_ctl -D /usr/local/var/postgres start` and 
`pgcli postgres://me:password@localhost:5432/diesel_demo`

## Some other same library ##

[sea-orm](https://github.com/SeaQL/sea-orm), looks like this one is async rather than the sync diesel.

More detail [Compare with Diesel](https://www.sea-ql.org/SeaORM/docs/internal-design/diesel/)
