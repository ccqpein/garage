use dioxus::prelude::*;

#[component]
pub fn app() -> Element {
    rsx! {
        document::Stylesheet {
            href: asset!("/assets/tailwind.css")
        }
    }
}
