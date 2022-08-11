package main

import (
	"log"
	"os"

	"github.com/urfave/cli/v2"
)

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
		},
		Action: func(ctx *cli.Context) error {
			makeAction(ctx.Int("concurrency-num"))
			return nil
		},
	}
}

func makeAction(c int) {

}

func main() {
	app := makeApp()

	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}
