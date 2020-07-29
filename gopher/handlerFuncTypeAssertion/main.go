package main

import "net/http"

func test1(w http.ResponseWriter, r *http.Request) {}

type a struct{}

func (a *a) ServeHTTP(w http.ResponseWriter, r *http.Request) {}

func main() {
	// Give type HandlerFunc, which implanmented ServeHTTP, so it becomes a Handler.
	// Also HandlerFunc is a type alias of function
	http.Handle("/1", http.HandlerFunc(test1))

	a := a{} // struct handler
	http.Handle("/2", &a)
}
