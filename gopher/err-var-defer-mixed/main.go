package main

import (
	"errors"
	"fmt"
)

func demo0() (err error) {
	err = errors.New("a")

	defer func() {
		fmt.Printf("in defer %+v\n", err)

		err = errors.New("c")
	}()

	err1 := errors.New("b")

	return err1
}

func demo1() (err error) {
	err = errors.New("a")

	err1 := errors.New("b")

	return err1
}

func main() {
	fmt.Printf("in main %+v\n", demo0())
	fmt.Printf("in main %+v\n", demo1())
}
