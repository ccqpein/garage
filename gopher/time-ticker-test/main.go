package main

import (
	"fmt"
	"time"
)

type S struct {
	ticker time.Ticker
}

// this test shows that ticker channel will always ok when I read
// until it close
func test1() {
	s := S{
		ticker: *time.NewTicker(time.Second),
	}

	fmt.Println("time now: ", time.Now())

	tick, ok := <-s.ticker.C
	fmt.Printf("read if it is ok right after created: %v, %v\n", tick, ok)

	select {
	case tick, ok := <-s.ticker.C:
		fmt.Printf("wait for the first ticker %v, %v\n", tick, ok)
		break
	}

	tick, ok = <-s.ticker.C
	fmt.Printf("read if it is ok right after first tick: %v, %v\n", tick, ok)

	s.ticker.Stop()

	// will panic
	//tick, ok = <-s.ticker.C
	//fmt.Printf("read if it is ok after stop: %v, %v\n", tick, ok)

	// also panic, interesting
	// look like after stop, s.ticker.C cannot be read
	// select {
	// case <-s.ticker.C:
	// 	fmt.Printf("read if it is ok after stop: %v, %v\n", tick, ok)
	// default:
	// 	fmt.Printf("in default branch")
	// }
}

// let me test the timer
// will be the deadlock. The timmer afterfunc return actually just for stop
func test2() {
	timer := time.AfterFunc(time.Second, func() {
		fmt.Println("after one second")
	})

	<-timer.C
}

// other timer test
// deadlock, looks like the timer.C cannot only read once
func test3() {
	timer := time.NewTimer(time.Second)

	<-timer.C
	<-timer.C
}

// other timer test
func test4() {
	timer := time.NewTimer(time.Second)
	for {
		select {
		case t := <-timer.C:
			fmt.Println("first timer: ", t)
			return
		default:
			fmt.Println("nope")
		}
		time.Sleep(time.Millisecond * 450)
	}
}

func main() {
	//test1()
	//test2()
	//test3()
	test4()
}
