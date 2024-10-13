package main

import (
	"fmt"
	"sync"
)

func test() {
	m := sync.Map{}
	m.Store(1, 1)
	done := make(chan struct{})
	go func() {
		if v, ok := m.LoadAndDelete(1); ok {
			fmt.Println("value is: ", v)
		} else {
			fmt.Println("dont get")
		}
		done <- struct{}{}
	}()

	go func() {
		if v, ok := m.LoadAndDelete(1); ok {
			fmt.Println("value is: ", v)
		} else {
			fmt.Println("dont get")
		}
		done <- struct{}{}
	}()

	for i := 0; i < 2; i++ {
		<-done
	}
}

func main() {
	for i := 0; i < 10; i++ {
		// always one get, one dont get
		test()
		fmt.Println("")
	}
}
