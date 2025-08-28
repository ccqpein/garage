package main

import (
	"context"
	"errors"
	"fmt"
)

type Step[T any] struct {
	v func(context.Context, T, error) (T, error)

	isCall      bool
	isErrHandle bool
}

type Chain[T any] struct {
	fs []Step[T]
}

func (c *Chain[T]) Next(f func(context.Context, T, error) (T, error)) *Chain[T] {
	c.fs = append(c.fs, Step[T]{v: f, isCall: true})
	return c
}

func (c *Chain[T]) IfError(f func(context.Context, T, error) (T, error)) *Chain[T] {
	c.fs = append(c.fs, Step[T]{v: f, isErrHandle: true})
	return c
}

func (c *Chain[T]) Run(ctx context.Context, init T) (T, error) {
	var err error
	var re T = init
	for _, f := range c.fs {
		if f.isCall {
			if err != nil {
				return re, fmt.Errorf("you forgot handle the err")
			}
			re, err = f.v(ctx, re, err)
			continue
		}

		if f.isErrHandle && err != nil {
			return f.v(ctx, re, err)
		}
	}

	return re, err
}

type A struct {
	a int
}

func main() {
	c := Chain[int]{}

	c.Next(func(_ context.Context, v int, e error) (int, error) {
		if v == 0 {
			return 0, errors.New("")
		}
		return v - 1, nil

	}).IfError(func(_ context.Context, v int, e error) (int, error) {
		return 0, errors.New("get error 0")

	}).Next(func(_ context.Context, v int, e error) (int, error) {
		if v == 0 {
			return 0, errors.New("")
		}
		return v - 1, nil

	}).IfError(func(_ context.Context, v int, e error) (int, error) {
		return 0, errors.New("get error 1")

	}).Next(func(_ context.Context, v int, e error) (int, error) {
		if v == 0 {
			return 0, errors.New("")
		}
		return v - 1, nil

	}).IfError(func(_ context.Context, v int, e error) (int, error) {
		return 0, errors.New("get error 2")
	})

	fmt.Println(c.Run(context.Background(), 2))
	fmt.Println(c.Run(context.Background(), 1))
	fmt.Println(c.Run(context.Background(), 5))
}
