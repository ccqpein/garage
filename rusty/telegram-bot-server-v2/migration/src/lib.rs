pub use sea_orm_migration::prelude::*;

mod m20220101_000001_create_table;
mod m20230625_181541_gpt_groups_whitelist;
mod m20230704_014130_reminder_table;
mod m20230818_010422_chat_records;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20220101_000001_create_table::Migration),
            Box::new(m20230625_181541_gpt_groups_whitelist::Migration),
            Box::new(m20230704_014130_reminder_table::Migration),
            Box::new(m20230818_010422_chat_records::Migration),
        ]
    }
}
