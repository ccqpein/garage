package main

import (
	"errors"
	"fmt"
)

type MyError struct {
	inner string
	err   error
}

func (e *MyError) Error() string {
	return e.inner + e.err.Error()
}

// new stuff from Go1.13
func (e *MyError) Unwrap() error {
	return e.err
}

func ReturnMyErr() error {
	err := &MyError{inner: "a", err: errors.New("aa")}
	return err
}

var err = ReturnMyErr()

func ReturnMyErrVar() error {
	return err
}

func IsAsTest() {
	err0 := &MyError{inner: "a", err: errors.New("aa")}

	if temp_e, ok := err.(*MyError); temp_e == err0 && ok {
		fmt.Printf("equal same, err: %v, err0: %v", err, err0)
	} else {
		// diff address
		fmt.Printf("ok?: %v\n", ok)                             // <-
		fmt.Printf("not equal, err: %p, err0: %p\n", err, err0) // <-
	}

	err1 := ReturnMyErrVar()

	if temp_e, ok := err.(*MyError); temp_e == err1 && ok {
		fmt.Printf("equal same, err: %v, err1: %v\n", err, err1) // <-
	} else {
		// diff address
		fmt.Printf("ok?: %v\n", ok)
		fmt.Printf("not equal, err: %p, err1: %p\n", err, err1)
	}

	// now try is
	if errors.Is(err1, err) {
		fmt.Print("ok\n") // <-
	} else {
		fmt.Print("not ok\n")
	}

	if errors.Is(err0, err) {
		fmt.Print("is ok\n")
	} else {
		fmt.Print("is not ok\n") // <-
	}

	// now try as
	var asTarget *MyError

	if errors.As(err0, &asTarget) {
		fmt.Print("as ok\n") // <-
	} else {
		fmt.Print("as not ok\n")
	}

	// test wrapping
	errW0 := fmt.Errorf("wrapping %w", err1)
	if errors.Is(errW0, err) {
		fmt.Print("wrap is ok\n") // <-
	} else {
		fmt.Print("not ok\n")
	}

	if errors.As(errW0, &asTarget) {
		fmt.Print("wrap as ok\n") // <-
	} else {
		fmt.Print("as not ok\n")
	}

	// test unwrap
	eInner := errors.New("inner error")
	errBox := &MyError{inner: "a", err: eInner}
	if errors.Is(errBox, eInner) {
		fmt.Print("box is ok\n") // <- it is ok because MyError has unwrap method
	} else {
		fmt.Print("box is not ok\n")
	}
}

func main() {
	IsAsTest()
}
