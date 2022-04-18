# README #

call rust lib in lisp (by cffi)

## tips ##

```toml
[lib]
crate-type = ["cdylib", "rlib"]
```

before run `cargo build`

`cffi:defcfun` doesn't have to have arguments of the function.

## external link ##

https://dev.to/veer66/calling-rust-from-common-lisp-45c5


