use std::{cell::RefCell, rc::Rc};

use crate::home::*;

use dioxus::prelude::*;
use dioxus_router::prelude::*;

/// An enum of all of the possible routes in the app.
#[derive(Routable, Clone)]
pub enum Route {
    #[route("/")]
    Home {},

    /// for some reason, I had to write all the props here
    #[route("/:title")]
    Blog { title: String },
}

// use dioxus::prelude::*;
// use dioxus_router::prelude::*;
// use std::str::FromStr;

// #[rustfmt::skip]
// #[derive(Clone, Debug, PartialEq, Routable)]
// pub enum Route {
//     #[nest("/blog")]
//         #[layout(Blog)]
//             #[route("/")]
//             BlogList {},

//             #[route("/:blog_id")]
//             BlogPost { blog_id: usize },
//         #[end_layout]
//     #[end_nest]
//     #[route("/")]
//     Index {},
// }

// #[component]
// fn App() -> Element {
//     rsx! {
//         Router::<Route> { }
//     }
// }

// #[component]
// fn Index() -> Element {
//     rsx! {
//         h1 { "Index" }
//         Link {
//             to: Route::BlogList {},
//             "Go to the blog"
//         }
//     }
// }

// #[component]
// fn Blog() -> Element {
//     rsx! {
//         h1 { "Blog" }
//         Outlet::<Route> { }
//     }
// }

// #[component]
// fn BlogList() -> Element {
//     rsx! {
//         h2 { "List of blog posts" }
//         Link {
//             to: Route::BlogPost { blog_id: 0 },
//             "Blog post 1"
//         }
//         Link {
//             to: Route::BlogPost { blog_id: 1 },
//             "Blog post 2"
//         }
//     }
// }

// #[component]
// fn BlogPost(blog_id: usize) -> Element {
//     rsx! {
//         h2 { "Blog Post" }
//     }
// }
