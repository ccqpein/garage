(ql:quickload "cffi")

(cffi:define-foreign-library hello-rust
  (t (:default "../rust-part/target/debug/librust_part"))) ;; careful the load path

(cffi:use-foreign-library hello-rust)

;; mod1 function
(cffi:defcfun ("hello_mod1" hello-rust-in-mod-1) :string)

(hello-rust-in-mod-1) 


;; mod2 function
(cffi:defcfun ("input_func_as_parameter" input-func-as-parameter-in-mod-2)
	:void
  (f :pointer)
  (n :int))

(input-func-as-parameter-in-mod-2 (lambda (x) (+ x 1)) 15)
