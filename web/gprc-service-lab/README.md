# README #

open two shell

**Golang part**

Generate go side code:

`protoc --proto_path=../protocols --go_out=. --go_opt=paths=import --go-grpc_out=. --go-grpc_opt=paths=import hello.proto`

Run go code:

`go run .`

**Rust part**

create folder for protocol code:

`md OUTPUT`

Run rust code:

`cargo run`

### Dependency ###

Golang side needs install some package

`go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@master`
`go install google.golang.org/protobuf/cmd/protoc-gen-go`

Rust need `build.rs`

