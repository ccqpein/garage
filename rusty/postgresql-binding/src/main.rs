use actix_web::{
    get, guard, http::header, web, App, HttpRequest, HttpResponse, HttpServer, Responder, Result,
};
use chrono::{DateTime, Utc};
use deadpool_postgres::{Client, Config, Manager, ManagerConfig, Pool, RecyclingMethod};
use serde::{Deserialize, Serialize};
use std::time::SystemTime;

use tokio_pg_mapper::FromTokioPostgresRow;
use tokio_pg_mapper_derive::PostgresMapper;

#[derive(Debug, Deserialize, Serialize, PostgresMapper)]
#[pg_mapper(table = "usersprofiles")]
pub struct UserProfile {
    pub user_profile_id: i32,
    pub user_id: i32,
    pub firstname: String,
    pub lastname: String,
    pub phone: String,
    pub nickname: String,
    pub imageurl: String,
    pub created_on: DateTime<Utc>,
}

impl UserProfile {
    #[inline]
    pub fn new() -> Self {
        UserProfile {
            user_profile_id: 0,
            user_id: 0,
            firstname: String::new(),
            lastname: String::new(),
            phone: String::new(),
            nickname: String::new(),
            imageurl: String::new(),
            created_on: DateTime::from(SystemTime::now()),
        }
    }
}

async fn handle(db_pool: &Pool) -> impl Responder {
    let client: Client = db_pool.get().await.unwrap();
    let stmt = client
        .prepare("select * from usersProfiles where user_profile_id = $1")
        .await
        .unwrap();
    let mut re = UserProfile::new();
    let rows = client
        .query(
            &stmt,
            &[
                &2,
                // &re.lastname,
                // &re.phone,
                // &re.nickname,
                // &re.imageurl,
                // &re.created_on,
            ],
        )
        .await
        .unwrap();
    // let rows = client.query(&stmt, &[]).await.unwrap();
    dbg!(&rows);
    rows.into_iter().for_each(|row| {
        re.user_profile_id = row.get(0);
        re.firstname = row.get(2)
    });

    // let rows = client
    //     .query_one(
    //         &stmt,
    //         &[
    //             &2,
    //             // &re.lastname,
    //             // &re.phone,
    //             // &re.nickname,
    //             // &re.imageurl,
    //             // &re.created_on,
    //         ],
    //     )
    //     .await
    //     .unwrap();
    //let re = UserProfile::from_row(rows).unwrap();

    dbg!(re);
    HttpResponse::Ok()
}

#[get("/index")]
async fn index(db_pool: web::Data<Pool>) -> impl Responder {
    handle(&db_pool).await
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    use tokio_postgres::NoTls;

    let mut cfg = Config::new();
    cfg.dbname = Some("postgres".to_string());
    //cfg.host = Some("localhost".to_string());
    //cfg.port = Some(5432);
    // cfg.manager = Some(ManagerConfig {
    //     recycling_method: RecyclingMethod::Fast,
    // });
    //cfg.password = Some("123456".to_string());
    dbg!(&cfg);
    let pool = cfg.create_pool(NoTls).expect("DB connection failed");
    println!("errrrrrr: {:?}", pool.status());
    //let client: Client = pool.get().await.expect("DB connection failed");

    HttpServer::new(move || App::new().data(pool.clone()).service(index))
        .bind("localhost:8080")?
        .run()
        .await;
    // let stmt = client
    //     .prepare("select * from usersProfiles where user_profile_id =2")
    //     .await
    //     .unwrap();
    // let mut re = UserProfile::new();
    // client
    //     .query(
    //         &stmt,
    //         &[
    //             &re.firstname,
    //             &re.lastname,
    //             &re.phone,
    //             &re.nickname,
    //             &re.imageurl,
    //             &re.created_on,
    //         ],
    //     )
    //     .await;

    // format!("{:?}", re);
    Ok(())
}

// use deadpool_postgres::{Config, Manager, ManagerConfig, Pool, RecyclingMethod};
// //use tokio::main;
// use tokio_postgres::NoTls;

// #[tokio::main]
// async fn main() {
//     let mut cfg = Config::new();
//     cfg.dbname = Some("postgres".to_string());
//     cfg.manager = Some(ManagerConfig {
//         recycling_method: RecyclingMethod::Fast,
//     });
//     let pool = cfg.create_pool(NoTls).unwrap();
//     for i in 1..10 {
//         let mut client = pool.get().await.unwrap();
//         let stmt = client.prepare("SELECT 1 + $1").await.unwrap();
//         let rows = client.query(&stmt, &[&i]).await.unwrap();
//         let value: i32 = rows[0].get(0);
//         assert_eq!(value, i + 1);
//     }
// }
