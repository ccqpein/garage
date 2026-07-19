use accountant::entry;
use accountant_rpc::Transaction;
use actix_web::{App, HttpResponse, HttpServer, Responder, post, web};
use anyhow::Context;
use clap::Parser;
use lisp_rpc_rust_raw_data::files::DataFile;
use lisp_rpc_rust_server::*;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    config_file: Option<PathBuf>,
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
    let config_file = args
        .config_file
        .unwrap_or_else(|| PathBuf::from("./accountant-config.lisprpc"));

    let config_file = DataFile::new(config_file)?;
    let data_folder = config_file
        .gen_table() // generate the whole table
        .get("config")
        .context("Cannot get expr data")?
        .get("data-folder")
        .context("Cannot get data-folder")?
        .to_string();

    println!(
        "Loading config full_path {:?}",
        data_folder.trim_matches('"')
    );

    let data_folder_path: PathBuf = data_folder.trim_matches('"').into();

    // 1. Setup the RPC Engine
    let server = RPCServer::new()
        .register::<Transaction, _>(move |mut tx: Transaction| {
            println!("Received Transaction via Actix: {:?}", tx);
            entry(data_folder_path.clone(), &mut tx)?;
            Ok(format!("Processed transaction: {:?}", tx.serialize_lisp()))
        })
        .map_err(|e| anyhow::anyhow!("RPC Registration Error: {}", e))?;

    println!("Starting Actix-web RPC Server on http://127.0.0.1:3388");

    // 2. Setup Actix-web Server
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(server.clone()))
            .route("/", web::get().to(hello))
            .service(rpc_handler)
    })
    .bind(("127.0.0.1", 3388))?
    .run()
    .await?;

    Ok(())
}
