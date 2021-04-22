package server

import "net/http"

type Foo struct{}

func (f *Foo) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte(`this is response`))
}
