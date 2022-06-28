/// https://www.grpc.io/docs/languages/go/quickstart/
package main

import (
	"bytes"
	"context"
	"crypto/tls"
	"crypto/x509"
	"log"
	"net"
	"os"
	pb "service-go/protocols/hello"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"
)

type server struct {
	pb.UnimplementedHelloWorldServer
}

func (s *server) SayHello(ctx context.Context, in *pb.HelloRequest) (*pb.HelloResponse, error) {
	log.Printf("Received: %v", in.GetClientType())
	return &pb.HelloResponse{MessageBody: "Receive from " + in.GetClientType() + " client"}, nil
}

func getClientTLSCert() tls.Certificate {
	cert, err := tls.LoadX509KeyPair("../../../rusty/mTCP-demo/ca/client_0.crt", "../../../rusty/mTCP-demo/ca/client_0.key")
	if err != nil {
		log.Fatal(err)
	}

	return cert
}

func clientRPC() {
	//conn, err := grpc.Dial("[::1]:9090", grpc.WithInsecure(), grpc.WithBlock(), grpc.WithTimeout(5*time.Second))

	rootCa := x509.NewCertPool()
	f, err := os.Open("../../../rusty/mTCP-demo/ca/ca.crt")
	if err != nil {
		log.Fatalf("cannot open file: %v", err)
	}
	defer f.Close()

	buf := new(bytes.Buffer)
	buf.ReadFrom(f)
	rootCrt, err := x509.ParseCertificate(buf.Bytes())
	if err != nil {
		log.Fatalf("cannot parser cert: %v", err)
	}

	rootCa.AddCert(rootCrt)
	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{getClientTLSCert()},
		RootCAs:      rootCa,
	}

	// certF, err := credentials.NewClientTLSFromFile("../../../rusty/mTCP-demo/ca/ca.crt", "")
	// if err != nil {
	// 	log.Fatalf("cannot read crt file: %v", err)
	// }

	conn, err := grpc.Dial("[::1]:9090", grpc.WithTransportCredentials(
		credentials.NewTLS(tlsConfig),
	), grpc.WithBlock(), grpc.WithTimeout(5*time.Second))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}

	defer conn.Close()

	client := pb.NewHelloWorldClient(conn)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	for {
		time.Sleep(2 * time.Second)
		r, err := client.SayHello(ctx, &pb.HelloRequest{ClientType: "Golang", MessageBody: "hello rust"})
		if err != nil {
			log.Printf("could not greet: %v", err)
		}

		log.Printf("Resp: %s", r.GetMessageBody())
	}
}

func main() {

	go clientRPC()

	// pure grpc server below
	lis, err := net.Listen("tcp", "localhost:9091")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	pb.RegisterHelloWorldServer(s, &server{})
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}

	// embed with http package
	/* // looks like http1 http package isn't work with s.ServeHTTP
	http.HandleFunc("/yoyoyo", s.ServeHTTP)
	log.Fatal(http.ListenAndServe(":9091", nil))
	*/
}
