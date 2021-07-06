/// https://www.grpc.io/docs/languages/go/quickstart/
package main

import (
	"context"
	"log"
	"net"
	pb "service-go/protocols/hello"
	"time"

	"google.golang.org/grpc"
)

type server struct {
	pb.UnimplementedHelloWorldServer
}

func (s *server) SayHello(ctx context.Context, in *pb.HelloRequest) (*pb.HelloResponse, error) {
	log.Printf("Received: %v", in.GetClientType())
	return &pb.HelloResponse{MessageBody: "Receive from " + in.GetClientType() + " client"}, nil
}

func clientRPC() {
	conn, err := grpc.Dial("[::1]:9090", grpc.WithInsecure(), grpc.WithBlock(), grpc.WithTimeout(5*time.Second))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}

	defer conn.Close()

	client := pb.NewHelloWorldClient(conn)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	for {
		time.Sleep(2 * time.Second)
		r, err := client.SayHello(ctx, &pb.HelloRequest{ClientType: "Golang", MessageBody: "empty"})
		if err != nil {
			log.Printf("could not greet: %v", err)
		}

		log.Printf("Resp: %s", r.GetMessageBody())
	}
}

func main() {

	go clientRPC()

	/// server
	lis, err := net.Listen("tcp", "localhost:9091")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	pb.RegisterHelloWorldServer(s, &server{})
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
