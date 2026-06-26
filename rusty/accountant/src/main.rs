use accountant_rpc::Transaction;
use actix_web::{post, web, App, HttpResponse, HttpServer, Responder};
use lisp_rpc_rust_server::*;

#[post("/rpc")]
async fn rpc_handler(body: String, server: web::Data<RPCServer>) -> impl Responder {
    match server.handle(&body) {
        Ok(response) => HttpResponse::Ok().body(response),
        Err(e) => HttpResponse::BadRequest().body(format!("RPC Error: {}", e)),
    }
}

async fn hello() -> impl Responder {
    HttpResponse::Ok().body("Hello from Lisp-RPC Server!")
}

#[actix_web::main]
async fn main() -> anyhow::Result<()> {
    env_logger::init();

    // Load configuration from accountant.toml
    let config = accountant::Config::load()?;
    println!("Successfully loaded config: {:?}", config);

    // 1. Setup the RPC Engine
    let server = RPCServer::new()
        .register::<Transaction, _>(|tx: Transaction| {
            println!("Received Transaction via Actix: {:?}", tx);
            Ok(format!("Processed transaction: {:?}", tx.serialize_lisp()))
        })
        .map_err(|e| anyhow::anyhow!("RPC Registration Error: {}", e))?;

    println!("Starting Actix-web RPC Server on http://127.0.0.1:8080");

    // 2. Setup Actix-web Server
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(server.clone()))
            .route("/", web::get().to(hello))
            .service(rpc_handler)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await?;

    Ok(())
}
