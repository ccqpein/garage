use std::env;

use sea_orm::{Database, DbErr};
use tokio::main;

#[main]
async fn main() -> Result<(), DbErr> {
    let current_dir = env::current_dir().unwrap();
    let db_path = "sqlite://".to_string()
        + &current_dir.into_os_string().into_string().unwrap()
        + "/db/sqlite.db?mode=rwc";

    let db = Database::connect(db_path).await?;
    Ok(())
}
