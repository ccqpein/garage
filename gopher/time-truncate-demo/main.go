package main

import (
	"fmt"
	"time"
)

func test1() {
	ticker := time.NewTicker(time.Second)
	count := 0
	for {
		if count == 10 {
			return
		}
		select {
		case t := <-ticker.C:
			tt := t.Truncate(time.Second)
			ttNow := time.Now().Truncate(time.Second)
			fmt.Printf("current time %+v equal ticker time %+v? %v\n", ttNow, tt, ttNow == tt)
			count += 1

		}

	}
}

func main() {
	test1()
}
