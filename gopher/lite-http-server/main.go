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
	http.Handle("/a/", http.StripPrefix("/a/", fs))
	http.Handle("/", fs)
	//http.Handle("/a/", http.StripPrefix("/a/", fs))

	http.ListenAndServe(":8081", nil)

}

// func main() {
// 	fileServer := http.Dir("../../rusty/lepto-life-game/dist/")
// 	http.HandleFunc("/a/", mimeTypeHandler(fileServer))
// 	http.HandleFunc("/", mimeTypeHandler(fileServer))

// 	http.ListenAndServe(":8081", nil)
// }

// func mimeTypeHandler(fs http.FileSystem) func(w http.ResponseWriter, r *http.Request) {
// 	return func(w http.ResponseWriter, r *http.Request) {
// 		if strings.HasSuffix(r.URL.Path, ".wasm") {
// 			w.Header().Set("content-type", "application/wasm")
// 		}
// 		http.FileServer(fs).ServeHTTP(w, r)
// 	}
// }
