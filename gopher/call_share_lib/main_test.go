package main

import (
	"io/ioutil"
	"os"
	"testing"
)

func BenchmarkParseMethods(b *testing.B) {
	body := []byte{}
	if f, err := os.Open("./methods.json"); err != nil {
		panic(err)
	} else {
		body, err = ioutil.ReadAll(f)
	}

	for i := 0; i < b.N; i++ {
		_ = ParseMethods(body)
	}
}
