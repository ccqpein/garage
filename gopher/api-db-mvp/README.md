# README #

Fibonacci sequence generator API

### Usage ###

Go version: `1.16`

First, you need put all your db configuration in `./dbconfig.json` like: 

```json
{
  "host": "localhost",
  "port": 5432,
  "user": "ccQ",
  "password": "123456",
  "dbname": "postgres"
}

```

Then run in terminal `go run .`. If your db config is somewhere else, you can run like `go run . --dbconfig={PG_CONFIG_PATH}/dbconfig.json`

This application will create one table `test` right after connect to DB for storing data. 

Then you can go to `http://localhost:8080/fib?input=10` to figure out the tenth number in fibonacci sequence.

### Build ###

Run `go build` will build this app to execution file.

### APIs ###

**/fib**

Arguments: `input`

Example: `/fib?input=55` will give you the 55th number in fibonacci sequence.

**/less**

Arguments: `than`

Example: `/less?than=120` will give you the number of how many numbers in fibonacci sequence are less than 120

**/clean**

Example: `/clean` will clean all memoizes result in DB

### Test ###

`go test ./...` in root directory will run all tests, including the `TestMain` in `main_test.go`. I use `github.com/ory/dockertest` in `main_test.go` for mocking database. Make sure you have `docker` running in your local machine.

### Benchmark test ###

There are two benchmark tests in `main_test.go`, run it with `go test -bench .` will run it.
