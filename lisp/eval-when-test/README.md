# README #

some fun tests of `eval-when`

1. when only `:compile-toplevel`, it eval when `(compile-file #P"./1.lisp")`
2. if `:compile-toplevel :load-toplevel`, it eval when `(compile-file #P"./1.lisp")` and `(load #P"./1.fasl")`. BUT NOT in `(load #P"./1.lisp")`
3. if `:execute`, it eval only when `(load #P"./1.lisp")`. 


for `2.lisp`

`C-c C-k` will print `:compile-toplevel` first and `:load-toplevel`.

`sbcl --noinform --load ./2.lisp` will print `:execute`
