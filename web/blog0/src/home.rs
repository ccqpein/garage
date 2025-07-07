use dioxus::prelude::*;

#[component]
pub fn Home() -> Element {
    let mut all_posts = use_signal(|| {
        //:= need to timer read the all files inside the posts
        vec!["aa".to_string(), "bb".to_string(), "cc".to_string()]
    });

    // read the posts from posts folder

    rsx! {
        for p in all_posts() {
            Blog{content: p},
            br{}
        }
    }
}

#[component]
fn Blog(content: String) -> Element {
    rsx! {"{content}"}
}
