package main

import (
	"log"
	"net/http"
)

func Test(w http.ResponseWriter, r *http.Request) {
	if pusher, ok := w.(http.Pusher); ok {
		options := &http.PushOptions{
			Header: http.Header{
				"Accept-Encoding": r.Header["Accept-Encoding"],
			},
		}
		// Push is supported.
		if err := pusher.Push("/test.js", options); err != nil {
			log.Printf("Failed to push: %v", err)
		}
	}

	w.Write([]byte(`<html>
<head>
	<title>Hello World</title>
	<script src="/test.js"></script>
</head>
<body>
Hello, gopher!
</body>
</html>`))
}

func TestNoPush(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte(`<html>
<head>
	<title>Hello World</title>
	<script src="/test.js"></script>
</head>
<body>
Hello, gopher! no push
</body>
</html>`))

}
