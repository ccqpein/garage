use actix_web::{web, App, Error, HttpResponse, HttpServer};
use clap::Parser;
use rustls::{Certificate, PrivateKey, ServerConfig};
use rustls_pemfile::{certs, pkcs8_private_keys};
use std::{fs::File, io::BufReader};
use telegram_bot::UpdateKind;
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server_v2::app::{App as botApp, AppLayer};
use telegram_bot_server_v2::*;
use tokio::runtime::{self, Runtime};
use tokio::sync::mpsc;
use tokio::sync::mpsc::Sender;
use tracing::{debug, info};

async fn default(web::Json(update): web::Json<Update>) -> HttpResponse {
    println!("update: {:?}", update);
    HttpResponse::Ok().body("")
}

async fn handler(
    web::Json(update): web::Json<Update>,
    _opts: web::Data<Opts>,
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

async fn making_app_layer(
    al: &mut AppLayer,
    rt: &Runtime,
    deliver_sender: &Sender<Msg2Deliver>,
    opts: &Opts,
) {
    // need make status checker first
    let mut status_checker = app::StatusChecker::new();
    // register first because status checker should be the first
    al.register_app(&status_checker);

    // make github commit check app
    // before echo
    let gc = app::GithubCommitCheck::new(deliver_sender.clone(), opts.vault.clone());
    al.register_app(&gc);
    rt.spawn(gc.run());

    // reminder app
    let reminder_app = app::Reminder::new(deliver_sender.clone(), status_checker.sender());
    status_checker
        .reminder_catcher(reminder_app.sender())
        .await
        .unwrap();
    al.register_app(&reminder_app);
    rt.spawn(reminder_app.run());

    // make echo
    // let echo = app::Echo::new(deliver_sender.clone());
    // al.register_app(&echo);
    // rt.spawn(echo.run());

    // make github commit check active
    let gca = app::GithubCommitCheckActive::new(deliver_sender.clone(), opts.vault.clone());
    al.register_app(&gca);
    rt.spawn(gca.run());

    rt.spawn(status_checker.run());
}

fn main() -> std::io::Result<()> {
    // tracing
    tracing::subscriber::set_global_default(
        tracing_subscriber::FmtSubscriber::builder()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
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
    let (mut applayer, app_sender) = app::AppLayer::new();

    rt.block_on(making_app_layer(&mut applayer, &rt, &deliver_sender, &opts));

    {
        // one thread runtime
        let local_rt = runtime::Builder::new_current_thread()
            .enable_all()
            .worker_threads(2)
            .build()?;

        std::thread::spawn(move || {
            let local = tokio::task::LocalSet::new();
            local.spawn_local(applayer.run());
            local_rt.block_on(local);
        });
    }

    actix_web::rt::System::new().block_on(async move {
        // SSL builder
        let cert_file = &mut BufReader::new(File::open(opts.vault.clone() + "/certs.pem").unwrap());
        let key_file = &mut BufReader::new(File::open(opts.vault.clone() + "/key.key").unwrap());
        let cert_chain = certs(cert_file)
            .unwrap()
            .into_iter()
            .map(|c| Certificate(c))
            .collect();
        let mut keys = pkcs8_private_keys(key_file).unwrap();
        let config = ServerConfig::builder()
            .with_safe_defaults()
            .with_no_client_auth()
            .with_single_cert(cert_chain, PrivateKey(keys.remove(0)))
            .expect("bad certificate/key");
        info!("done make tls config");

        // declare endpoint
        let endpoint = include_str!("../vault/endpoint");

        info!("start to run the httpserver on {}", endpoint);
        // start http server
        HttpServer::new(move || {
            App::new()
                //.data(Api::new(token))
                .app_data(web::Data::new(app_sender.clone()))
                .app_data(web::Data::new(opts.clone()))
                .route(endpoint, web::post().to(handler))
                .route(
                    // default
                    endpoint,
                    web::post().to(default),
                )
        })
        .bind_rustls("0.0.0.0:8443", config)
        .unwrap()
        .run()
        .await
    })
}
