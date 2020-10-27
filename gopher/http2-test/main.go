package main

import (
	"log"
	"net/http"
)

func main() {
	http.HandleFunc("/a", Test)
	http.HandleFunc("/b", TestNoPush)

	log.Fatal(http.ListenAndServeTLS(":9000", "cert.pem", "key.pem", nil))
}
