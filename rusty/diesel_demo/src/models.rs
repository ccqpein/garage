use super::schema::posts;
use diesel::Queryable;
use uuid::Uuid;

#[derive(Queryable)]
pub struct Post {
    pub id: i32,
    pub some_uuid: Uuid,
    pub title: String,
    pub body: String,
    pub published: bool,
}

// type DB = diesel::pg::Pg;

// impl Queryable<posts::SqlType, DB> for Post {
//     type Row = (i32, Uuid, String, String, bool);

//     fn build(row: Self::Row) -> diesel::deserialize::Result<Self> {
//         Ok(Self {
//             id: row.0,
//             some_uuid: row.1,
//             title: row.2,
//             body: row.3,
//             published: row.4,
//         })
//     }
// }

#[derive(Insertable)]
#[table_name = "posts"]
pub struct NewPost<'a> {
    pub title: &'a str,
    pub some_uuid: Uuid,
    pub body: &'a str,
}
