package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"sync"
	"time"

	"github.com/urfave/cli/v2"
)

type resultSet struct {
	succ, failed int
}

func (rs *resultSet) String() string {
	return fmt.Sprintf("succ: %d, failed: %d", rs.succ, rs.failed)
}

func makeApp() *cli.App {
	return &cli.App{
		Name:  "concurrency-http-call",
		Usage: "call the host",
		Flags: []cli.Flag{
			&cli.IntFlag{
				Name:    "concurrency-num",
				Aliases: []string{"c"},
				Value:   8,
				Usage:   "the number of how many concurrency client call",
			},
			&cli.IntFlag{
				Name:    "test-time",
				Aliases: []string{"t"},
				Value:   10,
				Usage:   "how long run the test",
			},
		},
		Action: func(c *cli.Context) error {
			fmt.Printf("%d client(s) run %d seconds\n", c.Int("concurrency-num"), c.Int("test-time"))

			ctx, cancel := context.WithTimeout(context.Background(),
				time.Duration(c.Int("test-time"))*time.Second)
			defer cancel()

			re := makeCall(ctx, c.Int("concurrency-num"), c.Args().Get(0))
			fmt.Println(re)
			return nil
		},
	}
}

func makeCall(ctx context.Context, c int, url string) *resultSet {
	testcaseChan := make(chan int, c)

	var wg sync.WaitGroup

	for i := 0; i < c; i++ {
		wg.Add(1)
		httpClient := http.Client{Timeout: time.Second}
		go testFunc(ctx, &httpClient, url, testcaseChan, &wg)
	}

	fmt.Println("make clients done")

	// handle the test result
	rs := resultSet{}
	flag := make(chan struct{})
	go func(testcaseChan <-chan int, finish chan<- struct{}) {
		for {
			select {
			case i, ok := <-testcaseChan:
				if !ok {
					flag <- struct{}{}
					return
				}
				if i == 0 {
					rs.succ += 1
				} else {
					rs.failed += 1
				}
			}
		}

	}(testcaseChan, flag)

	wg.Wait()
	close(testcaseChan)
	<-flag

	return &rs
}

func testFunc(ctx context.Context, client *http.Client, url string, testcaseChan chan<- int, wg *sync.WaitGroup) error {
	defer wg.Done()
	for {
		select {
		case <-ctx.Done():
			return nil
		default:
			resp, err := client.Get(url)
			if err != nil {
				testcaseChan <- 1
			}

			if resp.StatusCode != 200 {
				testcaseChan <- 1
			} else {
				testcaseChan <- 0
			}
		}
	}
}

func main() {
	app := makeApp()

	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}
