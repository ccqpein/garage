use std::error::Error;

use topcoat::{
    Result,
    context::Cx,
    router::{Router, RouterBuilderDiscoverExt, page},
    runtime::{Event, shard},
    view::{component, view},
};

#[tokio::main]
async fn main() {
    topcoat::start(Router::builder().discover().build())
        .await
        .unwrap();
}

#[page("/hello")]
async fn home() -> Result {
    view! {
        <!DOCTYPE html>
        <html>
            <body>
                hello(name: "World")
            </body>
        </html>
    }
}

#[component]
async fn hello(name: &str) -> Result {
    view! { <h1>"Hello, " (name) "!"</h1> }
}

// #[component]
// async fn search() -> Result {
//     view! {
//         signal query = String::new();

//         <input @input=$(|e: Event| query.set(e.target.value))>

//         // Updates as the user types.
//         search_results(query: $(query.get()))
//     }
// }

// #[shard]
// async fn search_results(cx: &Cx, query: String) -> Result {
//     view! {
//         <ul>
//             // Your own server-side code, like a database query:
//             for product in search_products(cx, &query).await? {
//                 <li>(product.name)</li>
//             }
//         </ul>
//     }
// }

// struct Product {
//     name: String,
// }

// async fn search_products(cx: &Cx, query: String) -> Result {
//     vec![
//         Product {
//             name: "a".to_string(),
//         },
//         Product {
//             name: "b".to_string(),
//         },
//     ]
// }
