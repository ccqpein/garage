package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/urfave/cli/v2"
)

type resultSet struct{}

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
			ctx, cancel := context.WithTimeout(context.Background(),
				time.Duration(c.Int("test-time"))*time.Second)
			defer cancel()

			_ = makeCall(ctx, c.Int("concurrency-num"), c.Args().Get(0))
			//:= TODO: printResult(result)
			return nil
		},
	}
}

func makeCall(ctx context.Context, c int, url string) *resultSet {
	// handle the test result
	//:= TODO: go func(){}

	testcaseChan := make(chan int, c)

	for i := 0; i < c; i++ {
		httpClient := http.Client{Timeout: time.Second}
		go testFunc(ctx, &httpClient, url, testcaseChan)
	}

	return nil
}

func testFunc(ctx context.Context, client *http.Client, url string, testcaseChan chan<- int) error {
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
