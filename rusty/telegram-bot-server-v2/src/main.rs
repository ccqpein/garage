use actix_web::{error, web, App, Error, HttpResponse, HttpServer};
use clap::Parser;
use rustls::internal::pemfile::{certs, pkcs8_private_keys};
use rustls::{NoClientAuth, ServerConfig};
use std::{fs::File, io::BufReader};
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server_v2::*;

fn main() -> std::io::Result<()> {
    // read token
    let mut lines = include_str!("../vault/telebottoken").lines();
    let token = lines.next().unwrap();
    let opts: Opts = Opts::parse();

    actix_web::rt::System::with_tokio_rt(|| tokio::runtime::Runtime::new().unwrap()).block_on(
        async move {
            let opts: Opts = Opts::parse();

            // SSL builder
            let mut config = ServerConfig::new(NoClientAuth::new());
            let cert_file =
                &mut BufReader::new(File::open(opts.vault.clone() + "/certs.pem").unwrap());
            let key_file =
                &mut BufReader::new(File::open(opts.vault.clone() + "/key.pem").unwrap());
            let cert_chain = certs(cert_file).unwrap();
            let mut keys = pkcs8_private_keys(key_file).unwrap();
            config.set_single_cert(cert_chain, keys.remove(0)).unwrap();

            // declare endpoint
            let endpoint = include_str!("../vault/endpoint");

            // tracing
            tracing::subscriber::set_global_default(
                tracing_subscriber::FmtSubscriber::builder()
                    .with_env_filter("telegram_bot=trace")
                    .finish(),
            )
            .unwrap();

            // start http server
            // HttpServer::new(move || {
            //     App::new()
            //         .data(Api::new(token))
            //         .data(opts.clone())
            //         .data(tx.clone())
            //         .route(endpoint, web::post().to(handler))
            // })
            // .bind_rustls("0.0.0.0:8443", config)
            // .unwrap()
            // .run()
            // .await
        },
    );
    Ok(())
}
