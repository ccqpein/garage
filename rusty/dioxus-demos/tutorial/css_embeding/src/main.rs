use chrono::Local;
use dioxus::prelude::*;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const HEADER_SVG: Asset = asset!("/assets/header.svg");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS } document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        OOO {}

    }
}

#[component]
fn OOO() -> Element {
    let mut is_expanded = use_signal(|| false);

    let current_timestamp = Local::now().format("%Y-%m-%d %H:%M:%S").to_string();

    rsx! {
        // This div centers the content on the screen and sets a background color.
        div {
            class: "flex flex-col items-center justify-center min-h-screen bg-gray-100 p-4",


            div {
                class: "bg-white p-6 rounded-lg shadow-md cursor-pointer w-80",

                onclick: move |_| is_expanded.set(!is_expanded()),

                // The title of the block.
                h2 {
                    class: "text-xl font-bold mb-2 text-center text-gray-800",
                    "Title"
                }

                // This `if` block conditionally renders the timestamp.
                // It will only be visible when `is_expanded` is `true`.
                if is_expanded() {
                    div {
                        // Tailwind CSS classes for the timestamp content:
                        // `mt-4 pt-4 border-t border-gray-200` adds space and a subtle border above.
                        // `text-gray-600` for text color, `text-sm` for smaller text, `text-center` for alignment.
                        class: "mt-4 pt-4 border-t border-gray-200 text-gray-600 text-sm text-center",

                        "Timestamp: {current_timestamp}"
                    }
                }
            }
        }
    }
}
