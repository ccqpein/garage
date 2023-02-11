package main

import (
	"net/http"
	"open-weather-api-call/src"
)

func handler(w http.ResponseWriter, r *http.Request) {
	queries := r.URL.Query()

	coord := queries.Get("coord")
	token := src.APIToken(queries.Get("token"))

	if req, err := src.NewWeatherRequest(coord, &token); err != nil {
		w.WriteHeader(503)
		w.Write([]byte("server wrong"))
	} else {
		if resp, err := src.CallAPI(req); err != nil {
			w.WriteHeader(503)
			w.Write([]byte("server wrong"))
		} else {
			w.Write(src.HandleResponse(resp).Byte())
		}
	}
}

func main() {
	http.HandleFunc("/", handler)
	err := http.ListenAndServe(":8080", nil)
	if err != nil {
		panic(err)
	}
}
