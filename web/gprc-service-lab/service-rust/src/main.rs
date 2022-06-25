use tokio::runtime;
use tokio::time::{sleep, Duration};
use tonic::{include_proto, transport::Server, Request, Response, Status};

pub mod hello {
    //tonic::include_proto!("hello");
    include!("../OUTPUT/hello.rs");
}

use hello::{
    hello_world_client::HelloWorldClient,
    hello_world_server::{HelloWorld, HelloWorldServer},
    *,
};

#[derive(Debug, Default)]
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

fn server_config() {}

//#[tokio::main]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let rt = runtime::Builder::new_multi_thread()
        .enable_time()
        .worker_threads(1) // just one thread
        .enable_io()
        .build()
        .unwrap();

    // client
    let client = HelloWorldClient::connect("http://localhost:9091");
    //let client = HelloWorldClient::connect("http://localhost:9091/yoyoyo"); // test golang grpc binding with http
    rt.spawn(async {
        sleep(Duration::from_secs(3)).await;
        let mut cc = match client.await {
            Ok(c) => c,
            Err(e) => panic!("{}", e),
        };
        loop {
            sleep(Duration::from_secs(3)).await;
            println!(
                "response: {:?}",
                cc.say_hello(tonic::Request::new(HelloRequest {
                    client_type: String::from("rust client"),
                    message_body: String::from("hello golang"),
                }))
                .await
            )
        }
    });

    // server
    let addr = "[::1]:9090".parse()?;
    let server = RustServer::default();

    rt.block_on(async {
        Server::builder()
            .accept_http1(true)
            .add_service(tonic_web::enable(HelloWorldServer::new(server)))
            .serve(addr)
            .await
            .unwrap()
    });

    Ok(())
}
