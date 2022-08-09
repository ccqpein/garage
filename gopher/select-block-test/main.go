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

func main() {
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
			// close channal gonna in this block
			println(ok)
			if !ok {
				time.Sleep(time.Second * 3)
				//after this, A 's channel isn't close
				// line 7 done until the line 28 consume ca
			}
			//return
		}
	}

}
