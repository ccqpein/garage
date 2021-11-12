package main

import (
	"fmt"
	"log"
	"net"
	"os"
	"strconv"
	"time"
)

func main() {
	arguments := os.Args
	if len(arguments) == 1 {
		fmt.Println("Please provide host:port.")
		return
	}

	p, err := strconv.ParseInt(arguments[1], 10, 64)
	log.Printf("local ip is 127.0.0.1 and port is %s", arguments[1])
	dialer := net.Dialer{
		LocalAddr: &net.TCPAddr{
			IP:   net.ParseIP("127.0.0.1"),
			Port: int(p),
		},
	}

	log.Printf("connect to %s", arguments[2])
	conn, err := dialer.Dial("tcp", arguments[2])
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	for i := 0; i < 10; i++ { // write three times
		_, err = conn.Write([]byte(arguments[3]))
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
