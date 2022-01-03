(ql:quickload "cffi")

(cffi:define-foreign-library hello-rust
  (t (:default "../rust-part/target/debug/librust_part"))) ;; careful the load path

(cffi:use-foreign-library hello-rust)

(cffi:defcfun ("hello_mod1" hello-rust-in-mod-1) :string)

(hello-rust-in-mod-1) 
