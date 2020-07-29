package main

type a struct{}

type b struct{}

type Test interface {
	TT()
}

func (a *a) TT() {}
func (a *b) TT() {}

type c interface{}

func main() {
	var empty_i interface{}
	print(c(empty_i)) // type convert, type to c

	a := a{}

	// a is interface {TT()}, so convert it to Test is fine
	// type convert grammar is same as function call
	print(Test(&a))

	// Test is interface, interface assertion grammar is different
	print(Test(&a).(*b))

	// this one does not work
	//print((*b)(Test(&a)))
}
