use chrono::Local;
use dioxus::prelude::*;

#[derive(Props, PartialEq, Debug, Clone)]
struct BlogProps {
    title: String,
    content: String,
    //createAt: String,
    this_post: Signal<Option<String>>,
}

#[component]
fn Blog(props: BlogProps) -> Element {
    rsx! {
        div {
            class: "bg-white p-6 rounded-lg shadow-md cursor-pointer w-80",

            onclick: move |_| props.this_post.set(Some(props.title.clone())),

            h2 {
                class: "text-xl font-bold mb-2 text-center text-gray-800",
                "{props.title}",
            }

            if props.this_post().is_some() && props.this_post().clone().unwrap() == title {
                div {
                    class: "mt-4 pt-4 border-t border-gray-200 text-gray-600 text-sm text-center",
                    "{content}",
                    br{}
                    "next",
                }
            }
        }
    }
}

// #[derive(Debug, Clone)]
// struct BlogList {
//     allBlogs: Vec<Blog>,
// }

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
        // for p in all_posts() {
        //     Blog{title: p.clone(), content: p, this_post:this_post}
        //     br{}
        // }

        BlogList{all_posts: all_posts(), this_post}
    }
}

#[component]
fn BlogList(all_posts: Vec<String>, this_post: Signal<Option<String>>) -> Element {
    if this_post().is_none() {
        return rsx! {
            for p in all_posts {
                Blog{title: p.clone(), content: p, this_post:this_post} //:= should be the title and click open the whole posts
                br{}
            }
        };
    } else {
        for p in all_posts {
            if p == this_post().clone().unwrap() {
                return rsx! {
                    Blog{title: p.clone(), content: p, this_post:this_post}
                };
            }
        }
        rsx!()
    }
}
