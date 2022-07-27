use actix_web::{web, App, HttpResponse, HttpServer};
//use Sentry::app::resume_service;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            //.service(resume_service)
            .default_service(web::to(|| HttpResponse::NotFound()))
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
