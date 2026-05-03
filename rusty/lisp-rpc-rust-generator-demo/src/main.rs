use actix_web::{App, HttpResponse, HttpServer, Responder, post, web};
use lisp_rpc_rust_generator_demo::rpc_server::RPCServer;
use lisp_rpc_rust_generator_demo::{ToRPCType, rpc_libs::*};
use std::io::{Error, ErrorKind};

/// Standard Actix handler that uses our RPCServer dispatcher
#[post("/rpc")]
async fn rpc_handler(body: String, server: web::Data<RPCServer>) -> impl Responder {
    match server.handle(&body) {
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
    env_logger::init();

    // call the init for register the right map type
    init();

    // 1. Setup the RPC Engine
    // RPCServer internal is already Arc-wrapped, so it's cheap to clone
    let server = RPCServer::new()
        .register::<GetBook, _>(|gb: GetBook| {
            println!("Received BookInfo via Actix: {:?}", gb);
            Ok(format!("Processed book: {:?}", gb.serialize_lisp()))
        })
        .map_err(|e| Error::new(ErrorKind::Other, e))?;

    println!("Starting Actix-web RPC Server on http://127.0.0.1:8080");

    // 2. Standard Actix-web Server Setup
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(server.clone()))
            .route("/", web::get().to(hello))
            .service(rpc_handler)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
