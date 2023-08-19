use sea_orm_migration::prelude::*;

use crate::m20230818_010422_chat_records::ChatRecords;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(ChatRecords::Table)
                    .add_column(ColumnDef::new(Alter::CreateAt).date_time().not_null())
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(ChatRecords::Table)
                    .drop_column(Alias::new("create_at"))
                    .to_owned(),
            )
            .await
    }
}
#[derive(Iden)]
enum Alter {
    CreateAt,
}
