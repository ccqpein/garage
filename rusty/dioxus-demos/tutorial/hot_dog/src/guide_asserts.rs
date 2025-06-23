use dioxus::prelude::{asset, Asset};

static CSS: Asset = asset!("/assets/main.css");

fn App() -> Element {
    rsx! {
        document::Stylesheet { href: CSS }
    }
}
