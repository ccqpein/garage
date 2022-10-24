package main

import "fmt"

type inner struct {
	v int
}

type A struct {
	i inner
}

func (a *A) return_inner() *inner {
	a.i.v = 1
	return &a.i
}

func main() {
	a := A{i: inner{v: 0}}

	a.return_inner() // v changed

	fmt.Printf("%+v\n", &a.i)
	fmt.Printf("%p\n", &a.i)
	fmt.Printf("%p\n", a.return_inner())
}
