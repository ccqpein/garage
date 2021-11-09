package main

import (
	"fmt"
	"log"
	"net"
	"os"
	"time"
)

func main() {
	arguments := os.Args
	if len(arguments) == 1 {
		fmt.Println("Please provide host:port.")
		return
	}

	conn, err := net.Dial("tcp", arguments[1])
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	for i := 0; i < 3; i++ { // write three times
		_, err = conn.Write([]byte(arguments[2]))
		if err != nil {
			log.Fatal(err)
			os.Exit(1)
		}

		log.Printf("Send %s, waiting for reply", arguments[2])

		// wait reply
		reply := make([]byte, 1024)

		_, err = conn.Read(reply)
		if err != nil {
			log.Fatal(err)
		}

		log.Printf("Server reply: %s", reply)
		time.Sleep(500 * time.Millisecond)
	}

	conn.Write([]byte("STOP"))
}
