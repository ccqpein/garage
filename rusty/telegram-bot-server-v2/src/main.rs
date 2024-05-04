use actix_web::{web, App, Error, HttpResponse, HttpServer};
use clap::Parser;
use rustls::{Certificate, PrivateKey, ServerConfig};
use rustls_pemfile::{certs, pkcs8_private_keys};
use sea_orm::{Database, DatabaseConnection};
use std::env;
use std::{fs::File, io::BufReader};
use telegram_bot::UpdateKind;
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server_v2::app::{App as botApp, AppLayer};
use telegram_bot_server_v2::util::FileDownloader;
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
    db: &DatabaseConnection,
    downloader: FileDownloader,
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
    let reminder_app = app::Reminder::new(
        deliver_sender.clone(),
        status_checker.sender(),
        opts.vault.clone(),
    );
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

    // make chat_gpt
    let chat_gpt = app::ChatGPT::new(
        deliver_sender.clone(),
        opts.vault.clone(),
        db,
        Some(downloader),
    )
    .await
    .unwrap();
    al.register_app(&chat_gpt);
    rt.spawn(chat_gpt.run());

    // make github commit check active
    let gca = app::GithubCommitCheckActive::new(deliver_sender.clone(), opts.vault.clone());
    al.register_app(&gca);
    rt.spawn(gca.run());

    rt.spawn(status_checker.run());
}

async fn making_app_layer_2(
    al: &mut AppLayer,
    rt: &Runtime,
    deliver_sender: &Sender<Msg2Deliver>,
    opts: &Opts,
    db: &DatabaseConnection,
    downloader: Option<FileDownloader>,
) {
    // make chat_gpt
    let chat_gpt = app::ChatGPT::new(deliver_sender.clone(), opts.vault.clone(), db, downloader)
        .await
        .unwrap();
    al.register_app(&chat_gpt);
    rt.spawn(chat_gpt.run());
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
    let token1 = lines.next().unwrap();

    let mut lines = include_str!("../vault/telebottoken-mark2").lines();
    let token2 = lines.next().unwrap();

    // tokio runtime
    let rt = tokio::runtime::Runtime::new().unwrap();

    // make deliver
    let (deliver_sender1, deliver_receiver1) = mpsc::channel::<Msg2Deliver>(5);
    let mut delvr1 = Deliver::new(Api::new(token1), deliver_receiver1);
    let downloader1 = FileDownloader::new(token1).unwrap();

    let (deliver_sender2, deliver_receiver2) = mpsc::channel::<Msg2Deliver>(5);
    let mut delvr2 = Deliver::new(Api::new(token2), deliver_receiver2);
    let downloader2 = FileDownloader::new(token2).unwrap();

    {
        rt.spawn(async move { delvr1.run().await });
        rt.spawn(async move { delvr2.run().await });
    }

    // make applayer
    let (mut applayer1, app_sender1) = app::AppLayer::new();
    let (mut applayer2, app_sender2) = app::AppLayer::new();

    rt.block_on(async {
        // connect the db
        // db file inside vault
        let db_path = "sqlite://".to_string() + &opts.vault + "/db/sqlite.db?mode=rwc";
        let db_conn = Database::connect(db_path).await.unwrap();

        making_app_layer(
            &mut applayer1,
            &rt,
            &deliver_sender1,
            &opts,
            &db_conn,
            downloader1,
        )
        .await;
        making_app_layer_2(
            &mut applayer2,
            &rt,
            &deliver_sender2,
            &opts,
            &db_conn,
            Some(downloader2),
        )
        .await;
    });

    {
        // one thread runtime
        let local_rt = runtime::Builder::new_current_thread()
            .enable_all()
            .worker_threads(2)
            .build()?;

        std::thread::spawn(move || {
            let local = tokio::task::LocalSet::new();
            local.spawn_local(applayer1.run());
            local.spawn_local(applayer2.run());
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
        let endpoint1 = include_str!("../vault/endpoint");
        let endpoint2 = include_str!("../vault/endpoint-mark2");

        info!("start to run the httpserver on {}", endpoint1);
        info!("start to run the httpserver on {}", endpoint2);
        // start http server
        HttpServer::new(move || {
            App::new()
                .service(
                    web::resource(endpoint1)
                        .app_data(web::Data::new(app_sender1.clone()))
                        .app_data(web::Data::new(opts.clone()))
                        .route(web::post().to(handler)),
                )
                .service(
                    web::resource(endpoint2)
                        .app_data(web::Data::new(app_sender2.clone()))
                        .app_data(web::Data::new(opts.clone()))
                        .route(web::post().to(handler)),
                )
                .default_service(web::post().to(default))
        })
        .bind_rustls("0.0.0.0:8443", config)
        .unwrap()
        .run()
        .await
    })
}
