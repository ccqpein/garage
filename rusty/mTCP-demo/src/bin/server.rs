use std::env;

use warp::Filter;

async fn run_server() {
    let routes = warp::any().map(|| "Hello, mTLS World!");

    warp::serve(routes)
        .tls()
        .key_path("ca/localhost.key")
        .cert_path("ca/localhost.bundle.crt")
        .client_auth_required_path("ca/ca.crt")
        .run(([0, 0, 0, 0], 3030))
        .await
}

#[tokio::main]
async fn main() {
    let args: Vec<String> = env::args().collect();
    let server = run_server();
    server.await;
}
