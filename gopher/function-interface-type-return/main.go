package main

type A interface {
	A() int
}

type Aa struct {
}

func (*Aa) A() int {
	return 1
}

func returnA() A {
	return &Aa{}
}

func returnAa() *Aa {
	return &Aa{}
}

func useA(a A) {}

/// more test
type B interface {
	B() A
}

type Bb struct {
}

// this one is wrong
// func (*Bb) B() *Aa {
// 	return &Aa{}
// }

func (*Bb) B() A {
	return &Aa{}
}

func useB(b B) {}

func main() {
	useA(returnA())

	a := Aa{}
	useA(&a)

	useA(returnAa())

	//
	b := Bb{}
	useB(&b)
}
