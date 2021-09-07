package main

import (
	"sync"
	"time"
)

type Sleeper struct{}

func (*Sleeper) sleep() {
	time.Sleep(5 * time.Second)
}

type Blocker struct {
	sync.Mutex
}

func (b *Blocker) block() {
	b.Lock()
	time.Sleep(5 * time.Second)
	b.Unlock()
}

type Wraper struct {
	s *Sleeper
	b *Blocker
}

func (w *Wraper) sleep(c chan int) {
	w.s.sleep()
	c <- 1
}

func (w *Wraper) block(c chan int) {
	w.b.block()
	c <- 2
}

func main() {
	s := Sleeper{}
	b := Blocker{}
	w := Wraper{&s, &b}
	c := make(chan int)
	go func() { w.sleep(c) }()
	go func() { w.sleep(c) }()
	go func() { w.block(c) }()
	go func() { w.block(c) }()

	now := time.Now()
	for i := 0; i < 4; i++ {
		println(<-c)
		println(time.Now().Sub(now))
	}
}
