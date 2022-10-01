use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
pub struct NewPost<'a> {
    pub title: &'a str,
    pub some_uuid: Uuid,
    pub body: &'a str,
}
