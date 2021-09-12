use axum::{extract, AddExtensionLayer, Router, Server};
use clap::Clap;
use rustls::internal::pemfile::{certs, pkcs8_private_keys};
use rustls::{NoClientAuth, ServerConfig};
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::Arc;
use std::time::Duration;
use std::{fs::File, io::BufReader};
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server::{app::*, init};
use tokio::{runtime, sync::mpsc::Sender};
use tower::ServiceBuilder;
use tower_http::compression::CompressionLayer;

async fn app(
    extract::Json(update): extract::Json<Update>,
    api: extract::Extension<Arc<Api>>,
    opts: extract::Extension<Arc<Opts>>,
    ch_sender: extract::Extension<Arc<Sender<Message>>>,
) -> &'static str {
    match update_router(update, &api, &opts, &ch_sender).await {
        Ok(_) => "",
        Err(_) => "inner problem",
    }
}

fn main() {
    let mut lines = include_str!("../../vault/telebottoken").lines();
    let token = lines.next().unwrap();

    let (mut watcher, mut deliver, mut reminder, tx) = init(Api::new(token));

    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap();

    {
        // one thread runtime
        let local_rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();

        std::thread::spawn(move || {
            let local = tokio::task::LocalSet::new();
            local.spawn_local(async move { watcher.run().await });
            local_rt.block_on(local);
        });
    }

    {
        let _ = rt.spawn(async move { deliver.run().await });
    }

    {
        let _ = rt.spawn(async move { reminder.run().await });
    }

    rt.block_on(async move {
        let opts: Opts = Opts::parse();

        // SSL builder
        let mut config = ServerConfig::new(NoClientAuth::new());
        let cert_file = &mut BufReader::new(File::open(opts.vault.clone() + "/certs.pem").unwrap());
        let key_file = &mut BufReader::new(File::open(opts.vault.clone() + "/key.pem").unwrap());
        let cert_chain = certs(cert_file).unwrap();
        let mut keys = pkcs8_private_keys(key_file).unwrap();
        config.set_single_cert(cert_chain, keys.remove(0)).unwrap();

        // declare endpoint
        let endpoint = include_str!("../../vault/endpoint");

        // tracing
        tracing::subscriber::set_global_default(
            tracing_subscriber::FmtSubscriber::builder()
                .with_env_filter("telegram_bot=trace")
                .finish(),
        )
        .unwrap();

        //
        let middleware_stack = ServiceBuilder::new()
            .timeout(Duration::from_secs(30))
            .concurrency_limit(100)
            .layer(CompressionLayer::new())
            .into_inner();

        // can I use rustls?
        let app = Router::new()
            .route(endpoint, axum::handler::post(app))
            .layer(AddExtensionLayer::new(Api::new(token)))
            .layer(AddExtensionLayer::new(opts.clone()))
            .layer(AddExtensionLayer::new(tx.clone()))
            .layer(middleware_stack);

        Server::bind(&SocketAddr::from((
            IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
            8443,
        )))
        .serve(app.into_make_service())
        .await
        .unwrap()
    })
}

// expand main macro
// tokio::runtime::Builder::new_multi_thread()
//     .enable_all()
//     .build()
//     .unwrap()
//     .block_on(async {})
