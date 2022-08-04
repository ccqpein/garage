use actix_web::{web, App, HttpResponse, HttpServer};
use Sentry::app::Resume;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    tracing::subscriber::set_global_default(
        tracing_subscriber::FmtSubscriber::builder()
            //:= need to learn tracing_subscriber
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .finish(),
    )
    .unwrap();

    let mut args = std::env::args();
    let vault_path: String = args
        .nth(1)
        .ok_or(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "need give the vault path",
        ))
        .unwrap();

    let resume_app = Resume::from_file_config(vault_path + "/resume_conf.json")
        .await
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

    HttpServer::new(move || {
        let mut app = App::new().default_service(web::to(|| HttpResponse::NotFound()));
        app = resume_app.register_resume_service(app);
        app
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
