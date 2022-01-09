use actix_web::{error, web, App, Error, HttpResponse, HttpServer};
use clap::Parser;
use rustls::{Certificate, ServerConfig};
use rustls_pemfile::{certs, pkcs8_private_keys};
use std::sync::Arc;
use std::{fs::File, io::BufReader};
use telegram_bot::UpdateKind;
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server_v2::*;
use tokio::sync::{mpsc, Mutex};
use tracing::info;

async fn handler(
    web::Json(update): web::Json<Update>,
    api: web::Data<Api>,
    opts: web::Data<Opts>,
    applayer: web::Data<Mutex<app::AppLayer>>,
) -> Result<HttpResponse, Error> {
    let reply = match update.kind {
        UpdateKind::Message(message) => {
            //:= I dont like this locker here
            applayer.get_ref().lock().await.consume_msg(message).await
        }
        _ => Err("Not support".to_string()),
    };

    match reply {
        Ok(_) => Ok(HttpResponse::Ok().body("")),
        Err(_) => Ok(HttpResponse::Ok().body("inner problem")),
    }
}

fn main() -> std::io::Result<()> {
    // read token
    let mut lines = include_str!("../vault/telebottoken").lines();
    let token = lines.next().unwrap();
    let opts: Opts = Opts::parse();

    // tokio runtime
    let rt = tokio::runtime::Runtime::new().unwrap();

    // make deliver
    let (deliver_sender, deliver_receiver) = mpsc::channel::<Msg2Deliver>(5);
    let mut delvr = Deliver::new(Api::new(token), deliver_receiver);

    {
        rt.spawn(async move { delvr.run().await });
    }

    // make applayer
    let mut applayer = app::AppLayer::new();

    // make echo
    let echo = app::Echo::new(deliver_sender);
    applayer.register_app(echo);

    actix_web::rt::System::with_tokio_rt(|| tokio::runtime::Runtime::new().unwrap()).block_on(
        async move {
            let opts: Opts = Opts::parse();

            // SSL builder
            //let mut config = ServerConfig::new(NoClientAuth::new());
            let cert_file =
                &mut BufReader::new(File::open(opts.vault.clone() + "/certs.pem").unwrap());
            let key_file =
                &mut BufReader::new(File::open(opts.vault.clone() + "/key.pem").unwrap());
            let cert_chain = certs(cert_file).unwrap();
            let mut keys = pkcs8_private_keys(key_file).unwrap();
            //config.set_single_cert(cert_chain, keys.remove(0)).unwrap();
            let mut config = ServerConfig::builder()
                .with_safe_default_cipher_suites()
                .with_safe_default_kx_groups()
                .with_safe_default_protocol_versions()
                .unwrap()
                .with_no_client_auth()
                .with_single_cert(cert_chain, keys.remove(0))
                .unwrap();

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
            HttpServer::new(move || {
                App::new()
                    .app_data(Api::new(token))
                    .app_data(opts.clone())
                    .route(endpoint, web::post().to(handler))
            })
            .bind_rustls("0.0.0.0:8443", config)
            .unwrap()
            .run()
            .await
        },
    );
    Ok(())
}
