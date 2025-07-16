use std::{cell::RefCell, collections::HashMap, rc::Rc};

use async_std::task::sleep;
use dioxus::prelude::*;

use crate::{
    blog_content::{all_blogs, Blog, Blogs},
    router::Route,
};

#[component]
pub fn BlogView(title: String) -> Element {
    rsx! {
        div {
            h2 {
                //class: "text-xl font-bold mb-2 text-center text-gray-800",
                "{title}",
            }


            div {
                //class: "mt-4 pt-4 border-t border-gray-200 text-gray-600 text-sm text-center",
                "{title} content",
                br{}
                "next",
            }

        }
    }
}

#[component]
pub fn Home() -> Element {
    let mut last_fire_time = use_signal(|| String::from("Never"));
    use_future(move || async move {
        loop {
            sleep(std::time::Duration::from_secs(30)).await;

            let now = chrono::Local::now();
            last_fire_time.set(format!("{}", now.format("%H:%M:%S")));
        }
    });

    let all_posts: Resource<Result<Blogs, ServerFnError>> = use_resource(move || async move {
        last_fire_time();
        all_blogs().await
    });

    let mut all_titles = use_signal(|| vec![]);
    use_effect(move || {
        let a = *all_posts.state().read();
        match a {
            UseResourceState::Ready => {
                let aa = &*all_posts.read_unchecked();
                match aa {
                    Some(m) => match m {
                        Ok(mm) => all_titles.set(mm.all_titles()),
                        Err(e) => {
                            tracing::error!("error: {e}")
                        }
                    },
                    None => {}
                }
            }
            _ => {}
        }
    });

    rsx! {
        for t in all_titles(){
            Link {
                to: Route::BlogView { title: t.clone() },
                "{t}"
            }
            br{}
        }
    }
}

//// === old one below ===

// #[derive(Props, PartialEq, Debug, Clone)]
// pub struct BlogProps {
//     title: String,
//     content: String,
//     //createAt: String,
//     this_post: Rc<RefCell<Signal<Option<String>>>>,
// }

// #[component]
// pub fn Blog(props: BlogProps) -> Element {
//     rsx! {
//         div {
//             class: "bg-white p-6 rounded-lg shadow-md cursor-pointer w-80",

//             onclick: move |_| {
//                 let tt = &props.title;
//                 tracing::debug!("click the {tt}") ;
//                 props.this_post.borrow_mut().set(Some(props.title.clone()))
//             },

//             h2 {
//                 class: "text-xl font-bold mb-2 text-center text-gray-800",
//                 "{props.title}",
//             }

//             if (props.this_post.borrow())().is_some() && (props.this_post.borrow())().clone().unwrap() == props.title {
//                 div {
//                     class: "mt-4 pt-4 border-t border-gray-200 text-gray-600 text-sm text-center",
//                     "{props.content}",
//                     br{}
//                     "next",
//                 }
//             }
//         }
//     }
// }

/// the botton of prev and next blog
#[component]
fn NextAndLastBlogButton(pre: Option<String>, next: Option<String>) -> Element {
    match (pre, next) {
        (Some(a), Some(b)) => {
            rsx! {div{"{a}"},div{"{b}"}}
        }
        (Some(a), None) => {
            rsx! {div{"{a}"},div{"nil"}}
        }
        (None, Some(b)) => {
            rsx! {div{"nil"},div{"{b}"}}
        }
        (None, None) => {
            rsx! {div{"nil"},div{"nil"}}
        }
    }
}

// #[derive(Props, PartialEq, Debug, Clone)]
// struct BlogViewProps {
//     all_posts: Vec<String>,
//     this_post: Rc<RefCell<Signal<Option<String>>>>,
// }

// #[component]
// fn BlogView(props: BlogViewProps) -> Element {
//     if (props.this_post).borrow()().is_none() {
//         return rsx! {
//             for p in props.all_posts {
//                 Blog{title: p.clone(), content: p, this_post:Rc::clone(&props.this_post)} //:= should be the title and click open the whole posts
//                 br{}
//             }
//         };
//     } else {
//         for (ind, p) in props.all_posts.iter().enumerate() {
//             if *p == (props.this_post).borrow()().unwrap_or("".to_string()) {
//                 return rsx! {
//                     Blog{title: p.clone(), content: p, this_post: Rc::clone(&props.this_post)}
//                     NextAndLastBlogButton{
//                         pre: if ind == 0 {
//                             None
//                         }else {
//                             props.all_posts.get(ind-1).cloned()
//                         },
//                         next: props.all_posts.get(ind + 1).cloned(),
//                     }
//                 };
//             }
//         }
//         rsx!()
//     }
// }

// #[component]
// pub fn Home() -> Element {
//     let mut all_posts = use_signal(|| vec!["aa".to_string(), "bb".to_string(), "cc".to_string()]);

//     let this_post = use_signal(|| Option::<String>::None);
//     use_effect(move || match this_post() {
//         Some(p) => {
//             tracing::debug!("{p}")
//         }
//         None => {
//             tracing::debug!("nothing")
//         }
//     });

//     rsx! {
//         BlogView{all_posts: all_posts(), this_post: Rc::new(RefCell::new(this_post))}
//     }
// }
