package subpkg

import "fmt"

func A(input map[string]int) int {
	sum := 0
	for s, a := range input {
		fmt.Print(s)
		sum += a
	}

	return sum
}
