use actix_web::{error, web, App, Error, HttpResponse, HttpServer};
use clap::Clap;
use futures::StreamExt;
use rustls::internal::pemfile::{certs, pkcs8_private_keys};
use rustls::{NoClientAuth, ServerConfig};
use std::{fs::File, io::BufReader};
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server::{app::*, deliver::Deliver, reminder::Reminder, watcher::Watcher};
use tokio::sync::mpsc::{self, Sender};

async fn handler(
    mut payload: web::Payload,
    api: web::Data<Api>,
    opts: web::Data<Opts>,
    ch_sender: web::Data<Sender<Message>>,
) -> Result<HttpResponse, Error> {
    // parse json now
    let mut body = web::BytesMut::new();
    while let Some(chunk) = payload.next().await {
        let chunk = chunk.map_err(|e| error::ErrorBadRequest(e.to_string()))?;

        if (body.len() + chunk.len()) > 262_144_usize {
            return Err(error::ErrorBadRequest(""));
        }

        body.extend_from_slice(&chunk);
    }

    let update = match serde_json::from_slice::<Update>(&body) {
        Ok(u) => u,
        Err(e) => {
            return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, e.to_string()).into())
        }
    };

    match update_router(update, &api, &opts, &ch_sender).await {
        Ok(_) => Ok(HttpResponse::Ok().body("")),
        Err(_) => Ok(HttpResponse::Ok().body("inner problem")),
    }
}

fn init() -> (Watcher, Deliver, Reminder) {
    todo!()
}

fn main() -> std::io::Result<()> {
    let (tx, rx) = mpsc::channel::<Message>(32);
    {
        let w = Watcher::new(rx);
        //:= watcher should run here
        let rt = actix_web::rt::Runtime::new().unwrap();
        let _ = rt.spawn(async move { w });
    }

    actix_web::rt::System::new().block_on(async move {
        let opts: Opts = Opts::parse();

        // SSL builder
        let mut config = ServerConfig::new(NoClientAuth::new());
        let cert_file = &mut BufReader::new(File::open(opts.vault.clone() + "/certs.pem").unwrap());
        let key_file = &mut BufReader::new(File::open(opts.vault.clone() + "/key.pem").unwrap());
        let cert_chain = certs(cert_file).unwrap();
        let mut keys = pkcs8_private_keys(key_file).unwrap();
        config.set_single_cert(cert_chain, keys.remove(0)).unwrap();

        // declare endpoint
        let endpoint = include_str!("../vault/endpoint");

        // read token
        let mut lines = include_str!("../vault/telebottoken").lines();
        let token = lines.next().unwrap();

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
                .data(Api::new(token))
                .data(opts.clone())
                .data(tx.clone())
                //:= status add here
                .route(endpoint, web::post().to(handler))
        })
        //.workers(1)
        //.bind_openssl("0.0.0.0:8443", builder)?
        .bind_rustls("0.0.0.0:8443", config)?
        .run()
        .await
    })
}
