package main

import "time"

func A(c chan int) {
	time.Sleep(time.Second * 2)
	c <- 1
	close(c)
	println("closed")
	time.Sleep(time.Second * 2)
	return

}

func B(c chan int) {
	time.Sleep(time.Second)
	close(c)
}

func AB() {
	ca := make(chan int)
	cb := make(chan int)
	go A(ca)
	go B(cb)

	for {
		select {
		case a, ok := <-ca:
			println("recieve", a, ok)
			time.Sleep(time.Second)
			return
		case _, ok := <-cb:
			// if cb isn't closed, it will wait; right after cb closed, it can get the value (default, false)
			// close channal gonna in this block
			println(ok)
			if !ok {
				time.Sleep(time.Second * 3)
				// after this, A 's channel isn't close
				// line 7 done until the line 28 consume ca
			}
			//return
		}
	}
}

func C() {
	c := make(chan int)
	go func(c chan int) {
		time.Sleep(time.Second)
		c <- 1
		println("can send, len c is", len(c)) // won't run because line 56 skipped
	}(c)

	go func(c chan int) {
		// wake up channel
		for i := 0; i < 2; i++ {
			select {
			case <-c:
				println("wake")
			default: // *if this default commented, <-c gonna wake the line 49*
			}
		}
		println("bye bye")
	}(c)

	time.Sleep(2 * time.Second)
	println("end")

	// println("stuff from channel?", <-c)
	// time.Sleep(time.Second)
}

func D() {
	a := make(chan int)
	b := make(chan int)
	c := make(chan int)
	go func() {
		defer func() {
			close(b)
			close(c)
			println("closed")
		}()

		time.Sleep(time.Second)
		println("after sleep")
		select {
		case <-a:
			println("return here")
			return
		case b <- 1:
		case c <- 1:
		}
	}()

	go func() {
		a <- 1
		a <- 1
	}()

	select {
	case <-a:
		println("break")
		break
	case <-b:
	case <-c:
	}

	time.Sleep(time.Second * 2)
}

func main() {
	//AB()
	C()
	//D()
}
