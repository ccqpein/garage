package main

import (
	"fmt"
	"time"

	"github.com/itchyny/timefmt-go"
)

func main() {
	now0 := time.Now().In(time.UTC).Format(time.RFC3339Nano)
	fmt.Println(now0)
	t0, err := timefmt.Parse(now0, "%Y-%m-%dT%H:%M:%S.%fZ")
	fmt.Println(t0, err)

	// now0 = time.Now().In(time.UTC).Format(time.RFC3339)
	// fmt.Println(now0)
	// t0, err = timefmt.Parse(now0, "%Y-%m-%dT%H:%M:%S.%fZ")
	// fmt.Println(t0, err)

	// t1, err := timefmt.Parse("Wed Jul 28 1993 14:39:07 GMT-0400 (Eastern Daylight Time)", "%a %b %d %Y %H:%M:%S %Z%z (%Z)")
	// fmt.Println(t1, err)

	t1, err := timefmt.Parse("Wed Jul 28 1993 14:39:07 GMT-0400", "%a %b %d %Y %H:%M:%S %Z%z")
	fmt.Println(t1, err)
}
