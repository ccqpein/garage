package main

import (
	"io"
	"os"
)

func aa() {
	destinationFile, errCreate := os.Create("~/Desktop/aaa")
	if errCreate != nil {
		panic(errCreate)
	}

	sourceFile, errOpen := os.Open("~/Desktop/bbb")
	if errOpen != nil {
		panic(errOpen)
	}

	_, errCopy := io.Copy(destinationFile, sourceFile) // Uses internal 32KB buffer
	if errCopy != nil {
		panic(errCopy)
	}
}

func main() {}
