package main

import (
	"fmt"
	"time"
)

// write the jitter that maybe cannot sure the jitter
func normalJitter() {
	fmt.Println("In the normalJitter jitter")

	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	a := time.Now()
	for i := 1; i <= 5; i++ {
		scheduledTime := <-ticker.C
		fmt.Printf("duration: %+v\n", scheduledTime.Sub(a))
		a = scheduledTime
	}
}

func otherJitter() {
	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	fmt.Println("Ticker started... (Waiting for the first 1s tick)")

	for i := 1; i <= 5; i++ {
		scheduledTime := <-ticker.C

		actualTime := time.Now()

		jitter := actualTime.Sub(scheduledTime)

		fmt.Printf("Tick #%d: Scheduled for %s | Woke up %d ns late (Jitter)\n",
			i,
			scheduledTime.Format("15:04:05.000"),
			jitter.Nanoseconds(),
		)
	}
}

func main() {
	normalJitter()
	otherJitter()
}
