package main

import (
	"fmt"
	"log"
	"net"
	"os"
	"time"
)

// can easy test by nc:
// echo -n "test out the server" | nc localhost 10000
func main() {
	arguments := os.Args
	if len(arguments) == 1 {
		fmt.Println("Please provide host:port.")
		return
	}

	l, err := net.Listen("tcp", arguments[1])
	if err != nil {
		log.Fatal(err)
	}

	defer l.Close()

	for {
		conn, err := l.Accept()
		if err != nil {
			log.Fatal(err)
		}

		go func(c net.Conn) {
			for {
				time.Sleep(2 * time.Second)
				buf := make([]byte, 1024)
				_, err := conn.Read(buf)
				if err != nil {
					log.Fatal(err)
				}

				log.Printf("receive message %s", buf)
				if string(buf[:4]) == "STOP" {
					log.Println("STOP received")
					c.Close()
					return
				}

				conn.Write([]byte("Message received."))
			}
		}(conn)
	}

}
