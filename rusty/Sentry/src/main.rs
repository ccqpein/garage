use actix_web::{web, App, HttpResponse, HttpServer};
use Sentry::app::Resume;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let resume_app = Resume::from_file_config("./resume-conf.lisp");

    // let mut app = App::new()
    //     //.service(resume_service)
    //     .default_service(web::to(|| HttpResponse::NotFound()));

    // app = resume_app.register_resume_service(app);
    HttpServer::new(move || {
        let mut app = App::new()
            //.service(resume_service)
            .default_service(web::to(|| HttpResponse::NotFound()));

        app = resume_app.register_resume_service(app);
        app
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
