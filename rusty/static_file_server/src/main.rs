use actix_files as fs;
use actix_web::http::header::{ContentDisposition, DispositionType};
use actix_web::{get, App, Error, HttpRequest, HttpServer};
use std::path::PathBuf;

#[get("/{test:.*}")]
async fn test(req: HttpRequest) -> Result<fs::NamedFile, Error> {
    let mut path: std::path::PathBuf = PathBuf::new();
    path.push(r"./static");
    path.push(req.match_info().query("test").parse::<String>().unwrap());

    let file = fs::NamedFile::open(path)?;
    Ok(file
        .use_last_modified(true)
        .set_content_disposition(ContentDisposition {
            disposition: DispositionType::Attachment,
            parameters: vec![],
        }))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // std::env::set_var("RUST_LOG", "actix_web=info");
    // env_logger::init();

    // curl localhost:8080/test1
    // curl localhost:8080/s/test1
    HttpServer::new(|| {
        App::new()
            .service(
                fs::Files::new("/s", "./static")
                    .show_files_listing()
                    .use_last_modified(true),
            )
            .service(test)
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}
