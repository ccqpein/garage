/// https://www.grpc.io/docs/languages/go/quickstart/
package main

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"log"
	"net"
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

func rootCA() tls.Certificate {
	cert, err := tls.LoadX509KeyPair("../../../rusty/mTCP-demo/ca/ca.crt", "../../../rusty/mTCP-demo/ca/ca.key")
	if err != nil {
		log.Fatal(err)
	}

	return cert
}

func clientRPC() {
	//:= old code without client auth
	//conn, err := grpc.Dial("[::1]:9090", grpc.WithInsecure(), grpc.WithBlock(), grpc.WithTimeout(5*time.Second))

	cert := getClientTLSCert()

	rootCa := x509.NewCertPool()
	leaf, err := x509.ParseCertificate(rootCA().Certificate[0])
	if err != nil {
		log.Fatalf("cannot set cert leaf: %v", err)
	}
	rootCa.AddCert(leaf)

	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{cert},
		RootCAs:      rootCa,
	}

	//:= update on 6/28/2022, dial address has to be the same as crt sign,
	//:= in this case, it is localhost:3000 as the mTCP-demo did
	conn, err := grpc.Dial("localhost:3000", grpc.WithTransportCredentials(
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

	//:= test lisp grpc, temp comment client call
	//:= go clientRPC()

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
