use actix_web::{error, web, App, Error, HttpResponse, HttpServer};
use clap::Parser;
use rustls::internal::pemfile::{certs, pkcs8_private_keys};
use rustls::{Certificate, NoClientAuth, PrivateKey, ServerConfig};
use std::sync::Arc;
use std::{fs::File, io::BufReader};
use telegram_bot::UpdateKind;
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server_v2::app::App as botApp;
use telegram_bot_server_v2::*;
use tokio::runtime;
use tokio::sync::mpsc::Sender;
use tokio::sync::{mpsc, Mutex};
use tracing::{debug, info};
use tracing_subscriber::field::debug;

async fn handler(
    web::Json(update): web::Json<Update>,
    opts: web::Data<Opts>,
    app_sender: web::Data<Sender<Message>>,
) -> Result<HttpResponse, Error> {
    match update.kind {
        UpdateKind::Message(message) => match app_sender.send(message).await {
            Ok(_) => Ok(HttpResponse::Ok().body("")),
            Err(e) => {
                debug!("{}", e);
                Ok(HttpResponse::Ok().body("inner problem"))
            }
        },
        _ => Ok(HttpResponse::Ok().body("Not support")),
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

    let opts: Opts = Opts::parse();

    // read token
    let mut lines = include_str!("../vault/telebottoken").lines();
    let token = lines.next().unwrap();

    // tokio runtime
    let rt = tokio::runtime::Runtime::new().unwrap();

    // make deliver
    let (deliver_sender, deliver_receiver) = mpsc::channel::<Msg2Deliver>(5);
    let mut delvr = Deliver::new(Api::new(token), deliver_receiver);

    {
        rt.spawn(async move { delvr.run().await });
    }

    // make applayer
    let (mut applayer, mut app_sender) = app::AppLayer::new();

    // make github commit check app
    // before echo
    let mut gc = app::GithubCommitCheck::new(deliver_sender.clone(), opts.vault.clone());
    applayer.register_app(&gc);
    rt.spawn(async move { gc.run().await });

    // make echo
    let mut echo = app::Echo::new(deliver_sender.clone());
    applayer.register_app(&echo);
    rt.spawn(async move { echo.run().await });

    {
        // one thread runtime
        let local_rt = runtime::Builder::new_current_thread()
            .enable_all()
            .worker_threads(2)
            .build()?;

        std::thread::spawn(move || {
            let local = tokio::task::LocalSet::new();
            local.spawn_local(async move { applayer.run().await });
            local_rt.block_on(local);
        });
    }

    actix_web::rt::System::builder()
        .build()
        .block_on(async move {
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
                    .data(app_sender.clone())
                    .data(opts.clone())
                    .route(endpoint, web::post().to(handler))
                    .route(
                        // default
                        endpoint,
                        web::post().to(|web::Json(update): web::Json<Update>| {
                            println!("update: {:?}", update);
                            HttpResponse::Ok().body("")
                        }),
                    )
            })
            .bind_rustls("0.0.0.0:8443", config)
            .unwrap()
            .run()
            .await
        })
}
