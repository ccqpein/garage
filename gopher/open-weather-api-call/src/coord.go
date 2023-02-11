package src

import (
	"errors"
	"strings"
)

func ParseCoord(coord string) (string, string, error) {
	a := strings.Split(coord, ",")
	if len(a) < 2 {
		return "", "", errors.New("parsing coord error")
	}

	return a[0], a[1], nil
}
