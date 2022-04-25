use diesel::prelude::*;
use diesel_demo::models::*;
use diesel_demo::schema::posts::dsl::*;
use diesel_demo::*;

extern crate diesel;

fn main() {
    let mut connection = establish_connection();
    let results = posts
        .filter(published.eq(true))
        .limit(5)
        .load::<Post>(&mut connection)
        .expect("Error loading posts");

    println!("Displaying {} posts", results.len());
    for post in results {
        println!("{}", post.title);
        println!("----------\n");
        println!("{}", post.body);
    }
}
