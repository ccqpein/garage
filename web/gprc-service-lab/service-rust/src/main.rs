use tokio::runtime;
use tokio::time::{sleep, Duration};
use tonic::transport::{Certificate, Identity, ServerTlsConfig};
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

fn server_tls_config() -> std::io::Result<ServerTlsConfig> {
    let client_ca_cert = std::fs::read("../../../rusty/mTCP-demo/ca/ca.crt")?;
    let client_ca_cert = Certificate::from_pem(client_ca_cert);

    let cert = std::fs::read("../../../rusty/mTCP-demo/ca/localhost.bundle.crt")?;
    let key = std::fs::read("../../../rusty/mTCP-demo/ca/localhost.key")?;
    let server_identity = Identity::from_pem(cert, key);

    Ok(ServerTlsConfig::new()
        .identity(server_identity)
        .client_ca_root(client_ca_cert))
}

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
    let addr = "127.0.0.1:3000".parse()?;
    //:= update on 6/28/2022, dial address has to be the same as crt sign,
    //:= in this case, it is localhost:3000 as the mTCP-demo did
    let server = RustServer::default();
    let tls_config = server_tls_config()?;

    rt.block_on(async {
        Server::builder()
            .tls_config(tls_config)
            .unwrap()
            .accept_http1(true)
            .add_service(tonic_web::enable(HelloWorldServer::new(server)))
            .serve(addr)
            .await
            .unwrap()
    });

    Ok(())
}
