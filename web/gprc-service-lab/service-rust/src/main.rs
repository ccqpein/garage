use tonic::{include_proto, transport::Server, Request, Response, Status};

pub mod hello {
    tonic::include_proto!("hello");
    //include!("../OUTPUT/hello.rs");
}

use hello::{hello_world_server::HelloWorld, *};

struct RustServer {}

#[tonic::async_trait]
impl HelloWorld for RustServer {
    async fn say_hello(
        &self,
        request: Request<HelloRequest>, // Accept request of type HelloRequest
    ) -> Result<Response<HelloResponse>, Status> {
        // Return an instance of type HelloReply
        println!("Got a request: {:?}", request);

        let reply = HelloResponse {
            message_body: format!("Hello {}!", request.into_inner().client_type).into(), // We must use .into_inner() as the fields of gRPC requests and responses are private
        };

        Ok(Response::new(reply)) // Send back our formatted greeting
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    //
    Ok(())
}
