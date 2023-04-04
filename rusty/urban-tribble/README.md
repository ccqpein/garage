# README #

## Usage ##

Need nightly rust

`env RUST_LOG=debug cargo run` 

In other terminal: 

**List api:** 

`curl 127.0.0.1:8080/list`

**Create:** 

```shell
curl -X POST \
-H "Accept: application/json" \
-H "Content-Type: application/json" localhost:8080/create \
-d '{"typ":"Fizz","exe_time":"2023-03-31 18:35:00"}'
```

Time has to future, or it will return error it. It will return the uuid of this task.

**Show**

`curl 127.0.0.1:8080/show/101a6d57-64c3-48f3-a5b3-919aeae55fc2`

**Delete**

`curl -X DELETE 127.0.0.1:8080/delete/101a6d57-64c3-48f3-a5b3-919aeae55fc2`
