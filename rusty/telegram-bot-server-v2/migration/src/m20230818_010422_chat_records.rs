use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(ChatRecords::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ChatRecords::Id)
                            .integer()
                            .not_null()
                            .auto_increment()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(ChatRecords::SpaceType).string().not_null())
                    .col(ColumnDef::new(ChatRecords::SpaceId).uuid().not_null())
                    .col(ColumnDef::new(ChatRecords::MessageID).uuid().not_null())
                    .col(ColumnDef::new(ChatRecords::ReplyTo).uuid())
                    .col(ColumnDef::new(ChatRecords::Role).string().not_null())
                    .col(ColumnDef::new(ChatRecords::Content).string())
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(ChatRecords::Table).to_owned())
            .await
    }
}

/// Learn more at https://docs.rs/sea-query#iden
#[derive(Iden)]
pub enum ChatRecords {
    Table,
    Id,

    //
    SpaceType, // group, private, super group
    SpaceId,

    MessageID,
    ReplyTo,

    Role,
    Content,
}
