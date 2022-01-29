use actix_web::{get, web, App, HttpRequest, HttpResponse, HttpServer};
use rustls::internal::pemfile::{certs, pkcs8_private_keys};
use rustls::{NoClientAuth, ServerConfig};
use std::{fs::File, io::BufReader};
use telegram_bot::{types::Update, Api, Message};

async fn handler(web::Json(update): web::Json<Update>) -> HttpResponse {
    println!("update: {:?}", update);
    HttpResponse::Ok().body("")
}

// async fn handler(req: HttpRequest) -> HttpResponse {
//     println!("{:?}", req);
//     HttpResponse::Ok().body("")
// }

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let mut config = ServerConfig::new(NoClientAuth::new());
    let cert_file = &mut BufReader::new(File::open("./vault/VA.pem").unwrap());
    let key_file = &mut BufReader::new(File::open("./vault/VA.key").unwrap());
    let cert_chain = certs(cert_file).unwrap();
    let mut keys = pkcs8_private_keys(key_file).unwrap();
    config.set_single_cert(cert_chain, keys.remove(0)).unwrap();

    let endpoint = include_str!("../vault/endpoint");
    let mut lines = include_str!("../vault/telebottoken").lines();
    let token = lines.next().unwrap();

    println!("endpoint: {}, token: {}", endpoint, token);

    HttpServer::new(|| {
        App::new()
            //.service(handler)
            .route(endpoint, web::post().to(handler))
    })
    .bind_rustls("0.0.0.0:8443", config)?
    //.client_timeout(50000)
    .run()
    .await
}
