package main

import (
	"errors"
	"fmt"
)

func generate(s string) (int, error) {
	return 1, errors.New(s)
}

func Test() (err error) {
	a, err := generate("err1")
	//if err != nil {
	//	return err // return before closure catching
	//}
	println(a)
	fmt.Printf("err1 address: %p\n", err)

	defer func() {
		fmt.Printf("inner in: %p\n", err) // empty err
		if err != nil {
			fmt.Println("which error", err.Error())
		} else {
			// this branch when this function return nil
			err = errors.New("inner")
		}
		fmt.Printf("inner out: %p\n", err)
	}()

	b, err := generate("err2")
	println(b)
	if err != nil {
		return err // this gonna catch in "which error" branch
	}

	fmt.Printf("err2 address: %p\n", err)

	return nil
}

func main() {
	fmt.Println("in main", Test())
}
