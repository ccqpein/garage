use std::{cell::RefCell, rc::Rc};

use dioxus::prelude::*;

use crate::{blog_content::all_blogs, router::Route};

#[component]
pub fn Blog(title: String) -> Element {
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
    // let mut all_posts = use_signal(|| vec!["aa".to_string(), "bb".to_string(), "cc".to_string()]);

    let mut last_fire_time = use_signal(|| String::from("Never"));
    spawn(async move {
        let mut interval = tokio::time::interval(tokio::time::Duration::from_secs(1));
        interval.tick().await;
        loop {
            // Wait for the next interval tick
            interval.tick().await;

            // --- Your code to run every 5 seconds goes here ---
            // Inside this loop, you can update state, make network requests, etc.

            // Example: Increment the tick count
            //ticks.set(*ticks.get() + 1);

            // Example: Update the last fire time
            let now = chrono::Local::now(); // Requires `chrono` crate `features = ["std"]`
            last_fire_time.set(format!("{}", now.format("%H:%M:%S")));

            println!("Timer fired! Ticks: {last_fire_time}");
            // --- End of your periodic code ---
        }
    });

    // use_effect(move || {
    //     // Spawn an asynchronous task using Tokio.
    //     // This task will run in the background without blocking the UI.
    //     let timer_task = spawn(async move {
    //         let mut interval = time::interval(Duration::from_secs(5));

    //         // Skip the first tick, which fires immediately.
    //         // We want the *first* actual action to happen *after* 5 seconds.
    //         interval.tick().await;

    //         loop {
    //             // Wait for the next interval tick
    //             interval.tick().await;

    //             // --- Your code to run every 5 seconds goes here ---
    //             // Inside this loop, you can update state, make network requests, etc.

    //             // Example: Increment the tick count
    //             ticks.set(*ticks.get() + 1);

    //             // Example: Update the last fire time
    //             let now = chrono::Local::now(); // Requires `chrono` crate `features = ["std"]`
    //             last_fire_time.set(format!("{}", now.format("%H:%M:%S")));

    //             println!("Timer fired! Ticks: {}", ticks());
    //             // --- End of your periodic code ---
    //         }
    //     });
    // });

    // let mut interval =
    // use_resource(
    //     move || async move { let ii = tokio::time::interval(tokio::time::Duration::from_secs(2));
    //                          loop {
    //                              ii.tick().await;
    //                              refresh
    //                          }
    //     },
    // );

    let all_posts = use_resource(move || async move {
        tracing::debug!("?");
        //tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
        //interval.tick().await;
        last_fire_time();
        tracing::debug!("{last_fire_time}");
        all_blogs().await
    });

    //let all_posts = resource.value();

    match all_posts.state().cloned() {
        UseResourceState::Pending => rsx! {
            "The resource is still pending"
        },
        UseResourceState::Paused => rsx! {
            "The resource has been paused"
        },
        UseResourceState::Stopped => rsx! {
            "The resource has been stopped"
        },
        UseResourceState::Ready => rsx! {
            "The resource is ready!"
        },
    }

    // match &*all_posts.read_unchecked() {
    //     Some(Ok(value)) => rsx! { "{value:?}" },
    //     Some(Err(err)) => rsx! { "Error: {err}" },
    //     None => rsx! { "Loading..." },
    // }

    // rsx! {
    //     // match all_posts() {
    //     //     Some(ps) => {

    //     //     },
    //     //     None => {}
    //     // }

    //     for t in all_posts.read_unchecked().clone().unwrap_or(vec![]){
    //         Link {
    //             to: Route::Blog { title: t.clone() },
    //             "{t}"
    //         }
    //         br{}
    //     }
    // }
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
