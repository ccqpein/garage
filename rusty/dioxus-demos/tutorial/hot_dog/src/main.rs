use dioxus::prelude::*;

////
use hot_dog::{guide_component::*, guide_state};
////

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const HEADER_SVG: Asset = asset!("/assets/header.svg");

mod guide_router;

use crate::guide_router::*;

fn main() {
    dioxus::launch(app);
}

fn app() -> Element {
    rsx! {
        document::Stylesheet { href: asset!("/assets/main.css") }
        "route here" br{}
        Router::<Route> {}
    }
}

// #[component]
// fn App() -> Element {
//     let show_title = true;
//     rsx! {
//         document::Link { rel: "icon", href: FAVICON }
//         document::Link { rel: "stylesheet", href: MAIN_CSS }
//         DogApp { breed: "corgi" }

//         br {}

//         {"Something"}

//         br {}

//         // Optionals
//         {show_title.then(|| rsx! { "title!" br{}} )}
//         for i in (0..5) {
//             "{i}" br{}
//         }
//         // And iterators
//         ul {
//             {(0..5).map(|i| rsx! { "{i}" })}
//         }

//         br{}

//         guide_state::DogView {  }
//     }
// }

// #[component]
// pub fn Hero() -> Element {
//     rsx! {
//         div {
//             id: "hero",
//             img { src: HEADER_SVG, id: "header" }
//             div { id: "links",
//                 a { href: "https://dioxuslabs.com/learn/0.6/", "📚 Learn Dioxus" }
//                 a { href: "https://dioxuslabs.com/awesome", "🚀 Awesome Dioxus" }
//                 a { href: "https://github.com/dioxus-community/", "📡 Community Libraries" }
//                 a { href: "https://github.com/DioxusLabs/sdk", "⚙️ Dioxus Development Kit" }
//                 a { href: "https://marketplace.visualstudio.com/items?itemName=DioxusLabs.dioxus", "💫 VSCode Extension" }
//                 a { href: "https://discord.gg/XgGxMSkvUM", "👋 Community Discord" }
//             }
//         }
//     }
// }
