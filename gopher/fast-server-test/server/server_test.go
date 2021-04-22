package server

import (
	"context"
	"io"
	"net/http"
	"testing"
	"time"
)

func TestHandler(t *testing.T) {
	a := Foo{}
	server := &http.Server{Addr: ":8080", Handler: &a}

	go func() {
		server.ListenAndServe()
	}()

	defer func() {
		server.Shutdown(context.Background())
		t.Log("closed server")
	}()

	// if I comment the line before
	// mac won't ask me "if I allow incoming network"
	time.Sleep(4 * time.Second)

	resp, err := http.Get("http://localhost:8080")
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}

	if string(body) != "this is response" {
		t.Failed()
	}
}
