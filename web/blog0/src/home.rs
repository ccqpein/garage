use dioxus::prelude::*;

#[component]
pub fn Home() -> Element {
    let mut all_posts = use_signal(|| {
        //:= need to timer read the all files inside the posts
        vec!["aa".to_string(), "bb".to_string(), "cc".to_string()]
    });

    let this_post = use_signal(|| Option::<String>::None);
    use_effect(move || match this_post() {
        Some(p) => {
            tracing::debug!("{p}")
        }
        None => {
            tracing::debug!("nothing")
        }
    });

    // let post = use_effect(|| {
    //     if this_post().is_none() {
    //         rsx! {
    //             for p in all_posts() {

    //                 Blog{title: p.clone(), content: p, this_post:this_post.clone()} //:= should be the title and click open the whole posts
    //                 br{}}
    //         }
    //     }
    //     // else {
    //     //     for p in all_posts() {
    //     //         if &p == this_post().as_ref().unwrap() {
    //     //             return rsx! {
    //     //                 Blog{title: p.clone(), content: p, this_post:this_post.clone()},
    //     //                 br {},
    //     //                 "haha"
    //     //             };
    //     //         }
    //     //     }
    //     // }
    // });

    rsx! {
        for p in all_posts() {
            Blog{title: p.clone(), content: p, this_post:this_post} //:= should be the title and click open the whole posts
            br{}
        }
    }
}

#[component]
fn Blog(title: String, content: String, this_post: Signal<Option<String>>) -> Element {
    rsx! {
        div {
            class: "bg-white p-6 rounded-lg shadow-md cursor-pointer w-80",

            onclick: move |_| this_post.set(Some(title.clone())),

            h2 {
                class: "text-xl font-bold mb-2 text-center text-gray-800",
                "{title}",
            }

            if this_post().is_some() && this_post().clone().unwrap() == title {
                div {
                    class: "mt-4 pt-4 border-t border-gray-200 text-gray-600 text-sm text-center",
                    "{content}",
                    "next",
                }
            }
        }
    }
}
