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

func demo2() (err error) {
	defer func() {
		fmt.Printf("in defer %+v\n", err)
	}()

	aErr := func() error {
		err := errors.New("in a")
		return err
	}()

	return aErr
}

func main() {
	//fmt.Printf("in main %+v\n", demo0())
	//fmt.Printf("in main %+v\n", demo1())
	fmt.Printf("in main %+v\n", demo2())
}
