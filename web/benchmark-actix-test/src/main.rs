use actix_web::{get, web, App, HttpResponse, HttpServer};

// #[get("/")]
// async fn index(web::Path((id, name)): web::Path<(u32, String)>) -> impl Responder {
//     format!("Hello {}! id:{}", name, id)
// }

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| App::new().service(web::resource("/").to(|| HttpResponse::Ok())))
        .bind("127.0.0.1:3001")?
        //.workers(4)
        .run()
        .await
}
