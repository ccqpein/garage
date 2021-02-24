use actix_web::{get, web, App, HttpServer};

struct AppState {
    app_name: String,
}

struct AppState2 {
    num: i64,
}

#[get("/")]
async fn index(data: web::Data<AppState>, data2: web::Data<AppState2>) -> String {
    let app_name = &data.app_name; // <- get app_name

    format!("Hello {}! {}", app_name, data2.num) // <- response with app_name
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .data(AppState {
                app_name: String::from("Actix-web"),
            })
            .data(AppState2 { num: 3333 })
            .service(index)
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}
