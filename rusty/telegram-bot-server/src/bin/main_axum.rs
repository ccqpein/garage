use axum::{body::Body, extract, http::Request, AddExtensionLayer, Router};
use clap::Clap;
use hyper::server::conn::Http;
use rustls::internal::pemfile::{certs, pkcs8_private_keys};
use rustls::{NoClientAuth, ServerConfig};
use std::sync::Arc;
use std::time::Duration;
use std::{fs::File, io::BufReader};
use telegram_bot::{types::Update, Api, Message};
use telegram_bot_server::{app::*, init};
use tokio::net::TcpListener;
use tokio::{runtime, sync::mpsc::Sender};
use tokio_rustls::TlsAcceptor;
use tower::ServiceBuilder;
use tower_http::compression::CompressionLayer;
use tracing::{debug, info};

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

// async fn app(mut req: Request<Body>) -> &'static str {
//     let full_body = hyper::body::to_bytes(&mut req.body_mut()).await.unwrap();
//     let update = serde_json::from_slice::<Update>(&full_body).unwrap();
//     let api = req.extensions().get::<Arc<Api>>().unwrap();
//     let opts = req.extensions().get::<Arc<Opts>>().unwrap();
//     let ch_sender = req.extensions().get::<Arc<Sender<Message>>>().unwrap();

//     match update_router(update, &api, &opts, &ch_sender).await {
//         Ok(_) => "",
//         Err(_) => "inner problem",
//     }
// }

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

        // tracing
        tracing::subscriber::set_global_default(
            tracing_subscriber::FmtSubscriber::builder()
                .with_env_filter("telegram_bot=trace")
                .finish(),
        )
        .unwrap();

        // SSL builder
        let mut config = ServerConfig::new(NoClientAuth::new());
        let cert_file = &mut BufReader::new(File::open(opts.vault.clone() + "/certs.pem").unwrap());
        let key_file = &mut BufReader::new(File::open(opts.vault.clone() + "/key.pem").unwrap());
        let cert_chain = certs(cert_file).unwrap();
        let mut keys = pkcs8_private_keys(key_file).unwrap();
        config.set_single_cert(cert_chain, keys.remove(0)).unwrap();
        config.set_protocols(&[b"h2".to_vec(), b"http/1.1".to_vec()]);

        let config = Arc::new(config);

        let acceptor = TlsAcceptor::from(config);
        let listener = TcpListener::bind(String::from("0.0.0.0:8443"))
            .await
            .unwrap();

        // declare endpoint
        let endpoint = include_str!("../../vault/endpoint");

        //
        let middleware_stack = ServiceBuilder::new()
            .timeout(Duration::from_secs(30))
            .concurrency_limit(100)
            .layer(CompressionLayer::new())
            .into_inner();

        let app = Router::new()
            .route(endpoint, axum::handler::post(app))
            .layer(AddExtensionLayer::new(Arc::new(Api::new(token))))
            .layer(AddExtensionLayer::new(Arc::new(opts.clone())))
            .layer(AddExtensionLayer::new(Arc::new(tx.clone())))
            .layer(middleware_stack);

        // https://github.com/tokio-rs/axum/blob/main/examples/low-level-rustls/src/main.rs#L36
        loop {
            let (stream, addr) = listener.accept().await.unwrap();
            debug!("listener accept from {}", addr);

            let acceptor = acceptor.clone();

            let app = app.clone();

            tokio::spawn(async move {
                match acceptor.accept(stream).await {
                    Ok(stream) => Http::new().serve_connection(stream, app).await.unwrap(),
                    Err(e) => println!("{:?}", e),
                }
            });
        }
    })
}

// expand main macro
// tokio::runtime::Builder::new_multi_thread()
//     .enable_all()
//     .build()
//     .unwrap()
//     .block_on(async {})
