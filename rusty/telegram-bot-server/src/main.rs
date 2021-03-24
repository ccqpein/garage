use actix_web::{error, web, App, Error, HttpResponse, HttpServer};
use clap::Clap;
use futures::StreamExt;
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod};
use telegram_bot::{types::Update, Api};
use telegram_bot_server::app::*;

async fn handler(
    mut payload: web::Payload,
    api: web::Data<Api>,
    opts: web::Data<Opts>,
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

    let update = serde_json::from_slice::<Update>(&body)?;

    match update_router(update, &api, &opts).await {
        Ok(_) => Ok(HttpResponse::Ok().body("")),
        Err(_) => Ok(HttpResponse::Ok().body("inner problem")),
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let opts: Opts = Opts::parse();
    // SSL builder
    let mut builder = SslAcceptor::mozilla_intermediate(SslMethod::tls()).unwrap();
    // read private key
    builder
        .set_private_key_file(opts.vault.clone() + "/key.pem", SslFiletype::PEM)
        .unwrap();
    // read certificate
    builder
        .set_certificate_chain_file(opts.vault.clone() + "/certs.pem")
        .unwrap();

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
            .route(endpoint, web::post().to(handler))
    })
    .bind_openssl("0.0.0.0:8443", builder)?
    .run()
    .await
}
