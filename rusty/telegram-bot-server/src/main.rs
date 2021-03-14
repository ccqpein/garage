use actix_web::{
    error, post, web, App, Error, FromRequest, HttpRequest, HttpResponse, HttpServer, Responder,
};

use futures::StreamExt;
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod, SslStream};
use std::io;
use std::io::prelude::*;
use std::net::{TcpListener, TcpStream};
use std::sync::Arc;
use std::thread;
use telegram_bot::types::Update;

async fn index(mut payload: web::Payload) -> Result<HttpResponse, Error> {
    // parse json now
    let mut body = web::BytesMut::new();
    while let Some(chunk) = payload.next().await {
        let chunk = chunk.map_err(|e| error::ErrorBadRequest(e.to_string()))?;

        if (body.len() + chunk.len()) > 262_144_usize {
            return Err(error::ErrorBadRequest(""));
        }

        body.extend_from_slice(&chunk);
    }

    println!(
        "receive {}",
        String::from_utf8(body.to_vec()).map_err(|e| error::ErrorBadRequest(e.to_string()))?
    );

    println!(
        "{:?}",
        //String::from_utf8(body.to_vec()).map_err(|e| error::ErrorBadRequest(e.to_string()))?
        serde_json::from_slice::<Update>(&body)?
    );

    Ok(HttpResponse::Ok().body("ok"))
}

async fn index2(mut payload: web::Payload) -> Result<HttpResponse, Error> {
    // parse json now
    let mut body = web::BytesMut::new();
    while let Some(chunk) = payload.next().await {
        let chunk = chunk.map_err(|e| error::ErrorBadRequest(e.to_string()))?;

        if (body.len() + chunk.len()) > 262_144_usize {
            return Err(error::ErrorBadRequest(""));
        }

        body.extend_from_slice(&chunk);
    }

    println!(
        "{:?}",
        //String::from_utf8(body.to_vec()).map_err(|e| error::ErrorBadRequest(e.to_string()))?
        serde_json::from_slice::<Update>(&body)?
    );

    Ok(HttpResponse::Ok().body("ok"))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let mut builder = SslAcceptor::mozilla_intermediate(SslMethod::tls()).unwrap();
    builder
        .set_private_key_file("./vault/key.pem", SslFiletype::PEM)
        .unwrap();
    builder
        .set_certificate_chain_file("./vault/certs.pem")
        .unwrap();

    let endpoint = include_str!("../vault/endpoint");

    //////////////////////

    HttpServer::new(move || App::new().route(endpoint, web::post().to(index2)))
        .bind_openssl("0.0.0.0:8443", builder)?
        .run()
        .await
}
