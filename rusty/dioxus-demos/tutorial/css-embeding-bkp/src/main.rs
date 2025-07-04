use chrono::Local;
use dioxus::prelude::*;

fn main() {
    launch(App);
}

#[component]
fn App() -> Element {
    let mut is_expanded = use_signal(|| false);

    let current_timestamp = Local::now().format("%Y-%m-%d %H:%M:%S").to_string();

    rsx! {
        div {
            class: "flex flex-col items-center justify-center min-h-screen bg-gray-100 p-4",

            div {
                // Tailwind CSS classes for styling: white background, padding, rounded corners, shadow.
                // `cursor-pointer` indicates that the element is clickable.
                // `w-80` sets a fixed width.
                class: "bg-white p-6 rounded-lg shadow-md cursor-pointer w-80",

                // The `onclick` handler toggles the `is_expanded` state.
                onclick: move |_| is_expanded.set(!*is_expanded.get()),

                // The title of the block.
                h2 {
                    class: "text-xl font-bold mb-2 text-center text-gray-800",
                    "Title"
                }

                // This `if` block conditionally renders the timestamp.
                // It will only be visible when `is_expanded` is `true`.
                if *is_expanded.get() {
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
