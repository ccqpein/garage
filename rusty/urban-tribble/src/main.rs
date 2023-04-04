use actix_web::{
    delete, error::ErrorNotFound, get, http::Error, post, web, App, HttpResponse, HttpServer,
    Responder,
};
use tokio::sync::mpsc;
use tracing::debug;
use urban_tribble::*;
use uuid::Uuid;

#[post("/create")]
async fn create(
    web::Json(d): web::Json<Task>,
    sender: web::Data<mpsc::Sender<Uuid>>,
) -> Result<HttpResponse, Error> {
    debug!("{}", serde_json::to_string(&d).unwrap());
    match Task::new(d, sender.as_ref().clone()).await {
        Ok(id) => Ok(HttpResponse::Ok().body(id.to_string())),
        Err(e) => Ok(HttpResponse::InternalServerError().body(format!("err: {}", e))),
    }
}

#[get("/list")]
async fn list() -> Result<HttpResponse, Error> {
    let body = list_all_tasks().await;
    Ok(HttpResponse::Ok().body(body))
}

#[get("/show/{id}")]
async fn show(
    id: web::Path<Uuid>,
    sender: web::Data<mpsc::Sender<Uuid>>,
) -> Result<HttpResponse, Error> {
    let id = id.into_inner();
    Ok(HttpResponse::Ok().body(show_task(id).await))
}

#[delete("/delete/{id}")]
async fn delete(
    id: web::Path<Uuid>,
    sender: web::Data<mpsc::Sender<Uuid>>,
) -> Result<HttpResponse, Error> {
    if delete_task(*id).await {
        Ok(HttpResponse::Ok().body("done"))
    } else {
        Ok(HttpResponse::Ok().body("cannot find it"))
    }
}

//#[actix_web::main]
fn main() -> std::io::Result<()> {
    // tracing
    tracing::subscriber::set_global_default(
        tracing_subscriber::FmtSubscriber::builder()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .finish(),
    )
    .unwrap();

    let (sender, receiver) = mpsc::channel::<Uuid>(20);

    let mut wrk = Worker::new(receiver);
    let rt = tokio::runtime::Runtime::new()?;

    // run the worker
    rt.spawn(async move { wrk.run().await });

    actix_web::rt::System::new().block_on(async move {
        HttpServer::new(move || {
            App::new()
                .service(create)
                .service(list)
                .service(show)
                .service(delete)
                .app_data(web::Data::new(sender.clone()))
                .default_service(web::to(|| HttpResponse::NotFound()))
        })
        .bind(("127.0.0.1", 8080))?
        .run()
        .await
    })
}
