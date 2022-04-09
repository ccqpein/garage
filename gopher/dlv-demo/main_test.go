package main

import "testing"

func TestWithoutSuit(t *testing.T) {
	ll := []int{1, 2, 3}
	for _, i := range ll {
		t.Log(i) // add break point here
	}
	t.Log("this is test without suit")
}

type WrapSuit struct{}
