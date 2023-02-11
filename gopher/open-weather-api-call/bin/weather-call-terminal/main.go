package main

import (
	"flag"
	"fmt"
	"log"

	"open-weather-api-call/src"
)

var (
	tokenFlag = flag.String("token", "", "Open weather api token")
	coord     = flag.String("coord", "40.7128,-74.0060", "Coordinate of weather. Ex. 40.7128,-74.0060")
)

func main() {
	flag.Parse()

	token := src.ReadAPIToken(*tokenFlag)

	log.Printf("weather api token: %s\n", token)
	log.Printf("coord: %s\n", *coord)

	if req, err := src.NewWeatherRequest(*coord, &token); err != nil {
		panic(err)
	} else {
		if resp, err := src.CallAPI(req); err != nil {
			panic(err)
		} else {
			fmt.Println(src.HandleResponse(resp))
		}
	}

}
