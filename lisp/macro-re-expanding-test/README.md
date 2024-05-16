# README #

try to write some test that when the macro expanding. 

Firstly, load the `main.lisp`

1. compile the whole file. `macro1` expanded
2. compile the `func1`, `macro1` expanded
3. run `(func1)`, no delay, just run
4. re-define the `macro1`, no delay

So the `macro1` expanding in compiling time of the caller. In this case, compiling the `func1`
