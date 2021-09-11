//use actix_web::{error, web, App, Error, HttpResponse, HttpServer};
use axum::{Router, Server};
use clap::Clap;
use futures::StreamExt;
use rustls::internal::pemfile::{certs, pkcs8_private_keys};
use rustls::{NoClientAuth, ServerConfig};
use std::{fs::File, io::BufReader};
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server::{
    app::*,
    deliver::{Deliver, Msg2Deliver},
    reminder::{Msg2Reminder, Reminder},
    watcher::Watcher,
};
use tokio::{
    runtime,
    sync::mpsc::{self, Sender},
};

async fn app()

fn main() -> std::io::Result<()> {
    let mut lines = include_str!("../../vault/telebottoken").lines();
    let token = lines.next().unwrap();

    let (mut watcher, mut deliver, mut reminder, tx) = init(Api::new(token));

    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()?;

    {
        // one thread runtime
        let local_rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

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

        // can I use rustls?
        let app = Router::new();
        Server::bind(SocketAddr::from("0.0.0.0:8443"));
    })
}

// expand main macro
// tokio::runtime::Builder::new_multi_thread()
//     .enable_all()
//     .build()
//     .unwrap()
//     .block_on(async {})
