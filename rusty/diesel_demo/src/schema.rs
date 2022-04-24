table! {
    posts (id) {
        id -> Int4,
        some_uuid -> Uuid,
        title -> Varchar,
        body -> Text,
        published -> Bool,
    }
}
