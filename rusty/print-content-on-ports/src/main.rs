use actix_web::{
    error, post, web, App, Error, FromRequest, HttpRequest, HttpResponse, HttpServer, Responder,
};
// use async_std::io::{Error, Result};
// use async_std::net::{TcpListener, TcpStream};
// use async_std::prelude::*;
use futures::StreamExt;
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod, SslStream};
use std::io;
use std::io::prelude::*;
use std::net::{TcpListener, TcpStream};
use std::sync::Arc;
use std::thread;

fn handle_client(stream: &mut SslStream<TcpStream>) {
    // let mut content = String::new();
    // stream.read_to_string(&mut content).unwrap();
    // println!("{:?}", content);
}

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
        "{}",
        String::from_utf8(body.to_vec()).map_err(|e| error::ErrorBadRequest(e.to_string()))?
    );

    Ok(HttpResponse::Ok().body(""))
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

    HttpServer::new(|| App::new().route("/", web::post().to(index)))
        .bind_openssl("0.0.0.0:8443", builder)?
        .run()
        .await

    //     let acceptor = Arc::new(acceptor.build());

    // let listener = TcpListener::bind("0.0.0.0:8443").await?;
    // let mut incoming = listener.incoming();
    // while let Some(stream) = incoming.next().await {
    //     match stream {
    //         Ok(stream) => {
    //             let mut stream_ = acceptor.accept(stream).unwrap();
    //             thread::spawn(move || {
    //                 //let mut stream = acceptor.accept(stream).unwrap();
    //                 handle_client(&mut stream_);
    //             });
    //         }
    //         Err(e) => { /* connection failed */ }
    //     }
    // }
    // Ok(())
}
