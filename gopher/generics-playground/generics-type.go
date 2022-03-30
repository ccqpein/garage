package main

import (
	"fmt"

	"golang.org/x/exp/constraints"
)

/*
https://go.dev/doc/tutorial/generics code demo below
*/

func SumIntsOrFloats[K comparable, V int64 | float64](m map[K]V) V {
	var s V
	for _, v := range m {
		s += v
	}
	return s
}

/*
below code example come from https://go.dev/blog/intro-generics
*/

// two ways to run this
// 1. x := GMin[int](2, 3)
// 2. x := GMin(2, 3)
// 3. fmin := GMin[float64]
//    m := fmin(2.71, 3.14)
func GMin[T constraints.Ordered](x, y T) T {
	if x < y {
		return x
	}
	return y
}

// Type parameters
type Tree[T interface{}] struct {
	left, right *Tree[T]
	value       T
}

func (t *Tree[T]) Lookup(x T) *Tree[T] { return nil }

var stringTree Tree[string]
var intTree Tree[int]

// Type set in type parameters
type Slice[T interface{ ~int | ~float64 }] struct {
	v []T
}

type Slice1[T interface{ String() }] struct {
	v []T
}

type Ss struct{}

func (Ss) String() {}

type S interface {
	String()
}

type Slice2[T interface{ S }] struct {
	v []T
}

type A interface{ aa() }

// cannot use main.S in union && cannot use main.A in union
//type Slice3[T interface{ S | A }] struct {
//	v []T
//}

type AS interface {
	S
	A
}

// AS is `and` of S A, failed example before is `or`
type Slice3[T interface{ AS }] struct {
	v []T
}

// Constraint type inference
// func Scale[E constraints.Integer](s []E, c E) []E {
// 	r := make([]E, len(s))
// 	for i, v := range s {
// 		r[i] = v * c
// 	}
// 	return r
// }

func Scale[S ~[]E, E constraints.Integer](s S, c E) S {
	r := make(S, len(s))
	for i, v := range s {
		r[i] = v * c
	}
	return r
}

func main() {
	fmt.Printf("SumIntsOrFloats: %v\n", SumIntsOrFloats(map[string]int64{"a": 1, "b": 2}))
	fmt.Printf("SumIntsOrFloats: %v\n", SumIntsOrFloats(map[string]float64{"a": 1, "b": 2}))

	fmt.Printf("Gmin: %v\n", GMin(2, 3))

	// error: cannot use generic type Slice[T interface{~int|~float64}] without instantiation
	//fmt.Printf("slice: %+v\n", Slice{v: []int{1, 2, 3}})
	fmt.Printf("slice: %+v\n", Slice[int]{v: []int{1, 2, 3}})

	fmt.Printf("slice: %+v\n", Slice1[Ss]{v: []Ss{{}}})
	fmt.Printf("slice: %+v\n", Slice2[Ss]{v: []Ss{{}}})
}
