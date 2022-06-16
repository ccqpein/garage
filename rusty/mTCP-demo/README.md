# README #

run service side:

`cargo build && ./target/debug/server`

run client side:

`cargo run`

## Test ##

after service run and `cd ./ca`: 

`curl -L  https://localhost:3030/ --cacert ca.crt --cert client_1.pem -v`
`curl -L  https://localhost:3030/ --cacert ca.crt --cert client_0.pem -v`
