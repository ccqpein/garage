use accountant_rpc::Transaction;
use actix_web::{App, HttpResponse, HttpServer, Responder, post, web};
use clap::Parser;
use lisp_rpc_rust_parser::data::GetAbleData;
use lisp_rpc_rust_server::*;
use std::fs::File;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Folder containing the configuration file
    #[arg(short, long)]
    folder: Option<PathBuf>,
}

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

    // Parse CLI arguments
    let args = Args::parse();
    let folder = args.folder.unwrap_or_else(|| PathBuf::from("."));

    // Open accountant-config.lisprpc in the specified folder
    let config_path = folder.join("accountant-config.lisprpc");
    let file = File::open(&config_path)
        .map_err(|e| anyhow::anyhow!("Failed to open config file at {:?}: {}", config_path, e))?;
    println!("Successfully opened config file: {:?}", config_path);

    let mut parser: lisp_rpc_rust_parser::Parser = Default::default();
    let config = parser.parse_root(file).map_err(|e| anyhow::anyhow!(e))?;

    println!("Successfully loaded config: {:?}", config);
    let data = lisp_rpc_rust_parser::data::Data::from_expr(&config[0]).unwrap();
    println!("Loading config data-folder: {:?}", data.get("data-folder"));

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
