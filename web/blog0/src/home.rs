use async_std::task::sleep;
use chrono::NaiveDate;
use dioxus::prelude::*;

use crate::{
    blog_content::{all_blogs, Blog, Blogs},
    router::Route,
};

static ALL_BLOGS: GlobalSignal<Blogs> = Global::new(|| Blogs::new());
static EMPTY_BLOG: GlobalSignal<Blog> = Global::new(|| Blog {
    title: "nil".to_string(),
    content: "nil".to_string(),
    date: NaiveDate::MIN,
});

#[component]
pub fn BlogView(title: String) -> Element {
    let posts: Resource<Result<Blogs, ServerFnError>> =
        use_resource(move || async move { all_blogs().await });

    use_effect(move || {
        let a = *posts.state().read();
        match a {
            UseResourceState::Ready => {
                let aa = &*posts.read_unchecked();
                match aa {
                    Some(m) => match m {
                        Ok(mm) => {
                            *ALL_BLOGS.write() = mm.clone();
                            //tracing::debug!("here inside? {:?}", ALL_BLOGS)
                        }
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

    let this_blog = ALL_BLOGS().all_blogs().get(&title).cloned();
    let prev = ALL_BLOGS().prev_blog(&title);
    let next = ALL_BLOGS().next_blog(&title);

    rsx! {
        div{
            class: "min-h-screen bg-gray-100 py-10 flex flex-col items-center",

            Link {
                class: "text-blue-600 hover:text-blue-800 text-lg mb-8 transition duration-300 ease-in-out",
                to: Route::Home {},
                "home"
            }

            div {
                class: "bg-white shadow-lg rounded-lg p-8 mx-4 sm:mx-auto max-w-3xl w-full",
                h2 {
                    class: "text-3xl sm:text-4xl font-extrabold text-gray-900 mb-6 text-center leading-tight",
                    {this_blog.clone().unwrap_or_else(|| EMPTY_BLOG()).title.clone()},
                }

                div {
                    class: "prose prose-lg max-w-none text-gray-700 leading-relaxed mb-8", // `prose` for markdown-like styling
                    dangerous_inner_html: {this_blog.unwrap_or_else(|| EMPTY_BLOG()).content.clone()},
                }
                br{}

                NextAndLastBlogButton{prev: prev, next: next}
            }
        }
    }
}

#[component]
pub fn Home() -> Element {
    let posts: Resource<Result<Blogs, ServerFnError>> =
        use_resource(move || async move { all_blogs().await });

    let mut all_titles = use_signal(|| vec![]);

    use_effect(move || {
        let a = *posts.state().read();
        match a {
            UseResourceState::Ready => {
                let aa = &*posts.read_unchecked();
                match aa {
                    Some(m) => match m {
                        Ok(mm) => {
                            all_titles.set(mm.all_titles());
                            *ALL_BLOGS.write() = mm.clone();
                        }
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
        div {
            class: "min-h-screen bg-gray-100 py-10 flex flex-col items-center",
            h1 {
                class: "text-4xl sm:text-5xl font-extrabold text-gray-900 mb-10 text-center leading-tight",
                "All Blog Posts"
            }
            div {
                class: "bg-white shadow-lg rounded-lg p-8 mx-4 sm:mx-auto max-w-2xl w-full",
                // Grid or flex layout for titles if you want multiple columns
                class: "grid grid-cols-1 gap-4",
                for t in all_titles(){
                    Link {
                        class: "block p-4 bg-blue-50 hover:bg-blue-100 rounded-md text-blue-800 \
                                text-lg font-medium transition duration-300 ease-in-out \
                                hover:shadow-md text-center",
                        to: Route::BlogView { title: t.clone() },
                        "{t}"
                    }
                    br{}
                }
            }
        }
    }
}

/// the botton of prev and next blog
#[component]
fn NextAndLastBlogButton(prev: Option<String>, next: Option<String>) -> Element {
    rsx! {
        div {
            class: "flex justify-between items-center mt-8 pt-4 border-t border-gray-200",

            // Previous Blog Link
            {
                if let Some(a) = prev {
                    rsx! {
                        Link {
                            to: Route::BlogView { title: a.clone()},
                            class: "px-6 py-3 bg-blue-500 text-white font-semibold rounded-lg \
                                    shadow-md hover:bg-blue-600 transition duration-300 ease-in-out \
                                    text-base sm:text-lg",
                            "← {a}"
                        }
                    }
                } else {
                    rsx! {
                        div {
                            class: "px-6 py-3 bg-gray-300 text-gray-600 font-semibold rounded-lg \
                                    cursor-not-allowed text-base sm:text-lg",
                            "← No Previous"
                        }
                    }
                }
            },

            // Next Blog Link
            {
                if let Some(b) = next {
                    rsx! {
                        Link {
                            to: Route::BlogView { title: b.clone() },
                            class: "px-6 py-3 bg-blue-500 text-white font-semibold rounded-lg \
                                    shadow-md hover:bg-blue-600 transition duration-300 ease-in-out \
                                    text-base sm:text-lg",
                            "{b} →"
                        }
                    }
                } else {
                    rsx! {
                        div {
                            class: "px-6 py-3 bg-gray-300 text-gray-600 font-semibold rounded-lg \
                                    cursor-not-allowed text-base sm:text-lg",
                            "No Next →"
                        }
                    }
                }
            }
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
