use dioxus::prelude::*;

#[component]
fn Home() -> Element {
    rsx! {
        for p in all_posts {}
    }
}
