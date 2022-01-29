use actix_web::{error, web, App, Error, HttpResponse, HttpServer};
use clap::Parser;
use rustls::internal::pemfile::{certs, pkcs8_private_keys};
use rustls::{Certificate, NoClientAuth, PrivateKey, ServerConfig};
use std::sync::Arc;
use std::{fs::File, io::BufReader};
use telegram_bot::UpdateKind;
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server_v2::*;
use tokio::sync::{mpsc, Mutex};
use tracing::{debug, info};
use tracing_subscriber::field::debug;

async fn handler(
    web::Json(update): web::Json<Update>,
    //api: web::Data<Api>,
    opts: web::Data<Opts>,
    applayer: web::Data<Mutex<app::AppLayer>>,
) -> Result<HttpResponse, Error> {
    info!("receive message: {:?}", update);
    let reply = match update.kind {
        UpdateKind::Message(message) => {
            //:= I dont like this locker here
            applayer.get_ref().lock().await.consume_msg(message).await
        }
        _ => Err("Not support".to_string()),
    };

    match reply {
        Ok(_) => Ok(HttpResponse::Ok().body("")),
        Err(e) => {
            debug!("{}", e);
            Ok(HttpResponse::Ok().body("inner problem"))
        }
    }
}

fn main() -> std::io::Result<()> {
    // tracing
    tracing::subscriber::set_global_default(
        tracing_subscriber::FmtSubscriber::builder()
            .with_env_filter("telegram_bot=trace")
            .finish(),
    )
    .unwrap();

    // read token
    let mut lines = include_str!("../vault/telebottoken").lines();
    let token = lines.next().unwrap();
    info!("token is: {}", token);
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
    applayer.register_app(echo); //:= TODO: return a channel for clone inside httpserver

    actix_web::rt::System::builder()
        .build()
        .block_on(async move {
            let opts: Opts = Opts::parse();
            info!("vault path: {}", opts.vault.clone());

            // SSL builder
            let mut config = ServerConfig::new(NoClientAuth::new());
            let cert_file =
                &mut BufReader::new(File::open(opts.vault.clone() + "/VA.pem").unwrap());
            let key_file = &mut BufReader::new(File::open(opts.vault.clone() + "/VA.key").unwrap());
            let cert_chain = certs(cert_file).unwrap();
            let mut keys = pkcs8_private_keys(key_file).unwrap();
            config.set_single_cert(cert_chain, keys.remove(0)).unwrap();
            info!("done make tls config");

            // declare endpoint
            let endpoint = include_str!("../vault/endpoint");

            info!("start to run the httpserver on {}", endpoint);
            // start http server
            HttpServer::new(move || {
                App::new()
                    //.data(Api::new(token))
                    .data(applayer)
                    .data(opts.clone())
                    .route(endpoint, web::post().to(handler))
            })
            .bind_rustls("0.0.0.0:8443", config)
            .unwrap()
            .run()
            .await
        })
}
