package main

import "fmt"

func main() {
	a := "yoyoyo"
	temp := []string{}
	for i := 0; i < 10; i++ {
		temp = append(temp, a)
	}

	fmt.Println(a)
}
