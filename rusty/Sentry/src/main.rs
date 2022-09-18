use actix_web::{web, App, HttpResponse, HttpServer};
use tracing_subscriber::filter::{EnvFilter, LevelFilter};
use Sentry::app::resume::Resume;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    tracing::subscriber::set_global_default(
        tracing_subscriber::FmtSubscriber::builder()
            .with_env_filter(
                EnvFilter::builder()
                    .with_default_directive(LevelFilter::DEBUG.into())
                    .from_env_lossy(),
            )
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
    .bind(("0.0.0.0", 8760))?
    .run()
    .await
}
