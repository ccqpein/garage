fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;
    use std::sync::LazyLock;

    use sea_orm::{ConnectionTrait, Database, DatabaseBackend, DatabaseConnection, Statement};
    use tokio::runtime::Runtime;
    use tracing::{debug, error, info};

    // this will block
    // static DB_CONNECTION: LazyLock<DatabaseConnection> = LazyLock::new(|| {
    //     dbg!("in db connecting");
    //     let re = Runtime::new().unwrap().block_on(async {
    //         Database::connect("postgres://test_user:test_password@localhost:5432/test_db")
    //             .await
    //             .expect("db connect error")
    //     });
    //     dbg!("connected");
    //     re
    // });

    lazy_static! {
        static ref DB_CONNECTION: DatabaseConnection = {
            dbg!("in db connecting");
            let re = Runtime::new().unwrap().block_on(async {
                Database::connect("postgres://test_user:test_password@localhost:5432/test_db")
                    .await
                    .expect("db connect error")
            });
            dbg!("connected");
            re
        };
    }

    fn use_connection(db: &DatabaseConnection) {}

    #[test]
    fn test() {
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::DEBUG)
            .with_test_writer()
            .init();

        // this block will dead lock
        // let DB_CONNECTION: LazyLock<DatabaseConnection> = LazyLock::new(|| {
        //     dbg!("in db connecting");
        //     let re = Runtime::new().unwrap().block_on(async {
        //         Database::connect("postgres://test_user:test_password@localhost:5432/test_db")
        //             .await
        //             .expect("db connect error")
        //     });
        //     dbg!("connected");
        //     re
        // });
        // LazyLock::force(&DB_CONNECTION);

        let rt = Runtime::new().unwrap();
        use_connection(&DB_CONNECTION);

        // code below is ok
        // let DB_CONNECTION = rt.block_on(async {
        //     Database::connect("postgres://test_user:test_password@localhost:5432/test_db")
        //         .await
        //         .expect("db connect error")
        // });

        rt.block_on(async {
            dbg!("in here?");
            let re = DB_CONNECTION
                .execute(Statement::from_string(
                    DatabaseBackend::Postgres,
                    "DELETE FROM test_table;",
                ))
                .await
                .unwrap();
            dbg!(re);
            dbg!("done");
        })
    }
}
