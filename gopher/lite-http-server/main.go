package main

import (
	"fmt"
	"net/http"
	"os"
)

func index(w http.ResponseWriter, r *http.Request) {
	m, _ := os.ReadFile("../../rusty/lepto-life-game/dist/index.html")
	fmt.Fprint(w, string(m))
}

func main() {
	fs := http.FileServer(http.Dir("../../rusty/lepto-life-game/dist/"))
	fmt.Printf("%+v\n", fs)
	http.Handle("/", fs)
	http.Handle("/a/", http.StripPrefix("/a/", fs))

	http.ListenAndServe(":8081", nil)

}
