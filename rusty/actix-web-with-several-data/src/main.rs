use std::sync::Mutex;

use actix_web::{get, web, App, HttpServer};
use tokio::time::{sleep, Duration};

struct AppState {
    app_name: String,
}

struct AppState2 {
    num: i64,
}

#[get("/read")]
async fn index(data: web::Data<Mutex<AppState>>, data2: web::Data<Mutex<AppState2>>) -> String {
    println!("trying to get lock in index");
    let app_name = &data.lock().unwrap().app_name; // <- get app_name

    // let app_name = match data.try_lock() {
    //     Ok(ref l) => l.app_name.clone(),
    //     Err(_) => "haha".to_string(),
    // };

    let num = match data2.try_lock() {
        Ok(ref l) => l.num.clone(),
        Err(_) => 0,
    };

    format!("Hello {}! {}", app_name, num) // <- response with app_name
}

#[get("/change")]
async fn change(data: web::Data<Mutex<AppState>>, data2: web::Data<Mutex<AppState2>>) -> String {
    println!("trying to get lock in change");
    let mut v = data2.lock().unwrap();
    //let v = data2;
    v.num += 1;

    sleep(Duration::from_secs(5)).await;

    format!("data2 change to {}", v.num)
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .data(Mutex::new(AppState {
                app_name: String::from("Actix-web"),
            }))
            .data(Mutex::new(AppState2 { num: 3333 }))
            // .data(AppState {
            //     app_name: String::from("Actix-web"),
            // })
            // .data(AppState2 { num: 3333 })
            .service(index)
            .service(change)
    })
    // all problem came from here
    // 1 worker means one thread, and mutex will block
    // thread and if two async function ask lock together,
    // it gonna dead lock
    .workers(1)
    .bind("127.0.0.1:8080")?
    .run()
    .await
}
