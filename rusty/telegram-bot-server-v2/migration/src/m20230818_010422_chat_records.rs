use sea_orm_migration::prelude::*;
use strum::{EnumIter, IntoEnumIterator};

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
                    .col(ColumnDef::new(ChatRecords::ChatSpace).string().not_null())
                    .col(ColumnDef::new(ChatRecords::ChatDetail).string().not_null())
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

type MessageID = String;
type GroupId = String;
type ChatId = String;
type SupergroupId = String;

/// Learn more at https://docs.rs/sea-query#iden
#[derive(Iden, EnumIter)]
enum ChatRecords {
    Table,
    Id,

    //
    ChatSpace {
        kind: ChatSpaceKind,
        id: MessageID, // message id
        reply_to: String,
    },

    ChatDetail {
        role: String,
        message: String,
    },
}

#[derive(Iden)]
enum ChatSpaceKind {
    Group(GroupId),
    Private(ChatId),
    SuperGroup(SupergroupId),
}

impl Default for ChatSpaceKind {
    fn default() -> Self {
        Self::Private(String::new())
    }
}
