use deadpool_postgres::Client;
use deadpool_postgres::Config;
use tokio_postgres::NoTls;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let mut cfg = Config::new();
    cfg.dbname = Some("postgres".to_string());
    cfg.host = Some("localhost".to_string());

    let pool = cfg.create_pool(NoTls).unwrap();
    let client: Client = pool.get().await.unwrap();
    let rows = client
        .query("select username from users", &[])
        .await
        .unwrap();
    println!("{:?}", rows[0]);
    rows.iter().for_each(|r| println!("{:?}", r));

    let rows = client
        .query_one(
            "select (user_id, username) from users where username = $1",
            &[&"nothing"],
        )
        .await
        .unwrap(); //panic here
    println!("{:?}", rows); // panic here
                            //rows.iter().for_each(|r| println!("{:?}", r));

    Ok(())
}
