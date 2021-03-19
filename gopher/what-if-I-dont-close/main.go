package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
)

func hh(w http.ResponseWriter, r *http.Request) {
	f0, err := os.OpenFile("notes.txt", os.O_RDWR|os.O_CREATE, 0755)
	if err != nil {
		log.Fatal(err)
	}

	// dont close file here and check if there is fd leaking
	fmt.Println(io.ReadAll(f0))
}

// func hh2(w http.ResponseWriter, r *http.Request) {
// 	f0, err := os.OpenFile("notes.txt", os.O_RDWR|os.O_CREATE, 0755)
// 	if err != nil {
// 		log.Fatal(err)
// 	}
// 	defer f0.Close()
// 	fmt.Println(io.ReadAll(f0))
// }

func main() {
	http.HandleFunc("/req", hh)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
