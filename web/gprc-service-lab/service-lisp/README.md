# README #

## Client Side ##

Use the [qitab/grpc](https://github.com/qitab/grpc) use the client library.

**install the protoc-gen-cl-pb**

[doc](https://github.com/qitab/cl-protobufs#installation). 

`cd protoc && PROTOC_ROOT=/usr/local make`

**git clone repo **

`git clone git@github.com:qitab/grpc.git ~/quicklisp/local-projects/grpc`

*issue happens*
~~[link](https://github.com/qitab/grpc/issues/44)~~ (resolved)

## usage ##

### tips ###

need `(ql:quickload "grpc")` and `(grpc:init-grpc)` first.

### generate the lisp code from protoc ###

`--plugin` need to be absolute path if failed.

`protoc --plugin=protoc-gen-cl-pb --cl-pb_out=output-file=hello-world.lisp:. --proto_path=../protocols hello.proto`


