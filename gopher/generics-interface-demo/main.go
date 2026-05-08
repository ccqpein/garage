package main

import "fmt"

// not generic interface
type Storage interface {
	Get()
	Insert()
}

type A struct{ Name string }
type B struct{ ID int }

type MyModels interface {
	A | B
}

type Wrap[T MyModels] struct {
	inner T
}

func (w Wrap[A]) Get() { fmt.Println("Get Wrap[A]") }

func (w Wrap[A]) Insert() { fmt.Println("Insert Wrap[A]") }

func entry(s Storage) {
	s.Get()
	s.Insert()
}

/////////////////////////////////////

type AStorage interface {
	Save(data A) error
}

func (w Wrap[T]) Save(data T) error {
	fmt.Println("Save Wrap[A]")
	return nil
}

func DoSomething(s AStorage) { fmt.Println("DoSomething") }

func main() {
	// even the Wrap[B] can handle by entry
	entry(Wrap[A]{inner: A{
		Name: "",
	}})

	entry(Wrap[B]{inner: B{
		ID: 1,
	}})

	//
	wrapA := &Wrap[A]{inner: A{
		Name: "",
	}}
	DoSomething(wrapA)

	// wrapB := &Wrap[B]{inner: B{
	// 	ID: 1,
	// }}
	// DoSomething(wrapB) // this one gonna fail
}
