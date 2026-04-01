use actix_web::{App, HttpResponse, HttpServer, Responder, post, web};
use lisp_rpc_rust_generator_demo::rpc_libs::*;
use lisp_rpc_rust_generator_demo::rpc_server::RPCServer;
use std::sync::Arc;

/// Standard Actix handler that uses our RPCServer dispatcher
#[post("/rpc")]
async fn rpc_handler(body: String, server: web::Data<Arc<RPCServer>>) -> impl Responder {
    match server.dispatch(&body) {
        Ok(response) => HttpResponse::Ok().body(response),
        Err(e) => HttpResponse::BadRequest().body(format!("RPC Error: {}", e)),
    }
}

/// A simple hello handler for testing
async fn hello() -> impl Responder {
    HttpResponse::Ok().body("Hello from Lisp-RPC Server!")
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // 1. Setup the RPC Engine (now non-generic)
    let server = Arc::new(RPCServer::new().register::<GetBook, _>(|info| {
        println!("Received BookInfo via Actix: {:?}", info);
        Ok(format!("Processed book: {}", info.title))
    }));

    println!("Starting Actix-web RPC Server on 127.0.0.1:8080");

    // 2. Standard Actix-web Server Setup
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(server.clone())) // Inject the RPC engine
            .route("/", web::get().to(hello))
            .service(rpc_handler)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
