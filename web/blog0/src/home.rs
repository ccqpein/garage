use chrono::NaiveDate;
use dioxus::prelude::*;
use pulldown_cmark::{html, Options, Parser};

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

    // render the markdown
    let rendered_content = if let Some(blog) = this_blog.as_ref() {
        let markdown_input = &blog.content;
        let mut options = Options::empty();
        options.insert(Options::ENABLE_TABLES);
        options.insert(Options::ENABLE_FOOTNOTES);
        options.insert(Options::ENABLE_STRIKETHROUGH);
        options.insert(Options::ENABLE_TASKLISTS);
        options.insert(Options::ENABLE_SMART_PUNCTUATION);

        let parser = Parser::new_ext(markdown_input, options);

        let mut html_output = String::new();
        html::push_html(&mut html_output, parser);
        html_output
    } else {
        // Fallback for empty blog content or loading state
        EMPTY_BLOG.read().content.clone()
    };

    rsx! {
        div {
            class: "min-h-screen bg-gray-100 dark:bg-gray-900 py-10 flex flex-col items-center",

            Link {
                to: Route::Home {},
                class: "text-blue-600 hover:text-blue-800 dark:text-blue-400 dark:hover:text-blue-200 text-lg mb-8 transition duration-300 ease-in-out",
                "← Back to Home"
            }

            div {
                class: "bg-white dark:bg-gray-800 shadow-lg rounded-lg p-8 mx-4 sm:mx-auto max-w-3xl w-full",

                h2 {
                    class: "text-3xl sm:text-4xl font-extrabold text-gray-900 dark:text-white mb-6 text-center leading-tight",
                    {this_blog.clone().unwrap_or_else(|| EMPTY_BLOG.read().clone()).title.clone()},
                }

                div {
                    class: "prose prose-lg max-w-none text-gray-700 leading-relaxed mb-8 dark:prose-invert",
                    dangerous_inner_html: "{rendered_content}",
                }

                NextAndLastBlogButton{ prev: prev, next: next }
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
            class: "min-h-screen bg-gray-100 dark:bg-gray-900 py-10 flex flex-col items-center",
            h1 {
                class: "bg-green-500 text-blue-500",
                class: "text-4xl sm:text-5xl font-extrabold text-gray-900 dark:text-white mb-10 text-center leading-tight",
                "All ccQ Blog Posts"
            }
            div {
                class: "bg-white dark:bg-gray-800 shadow-lg rounded-lg p-8 mx-4 sm:mx-auto max-w-2xl w-full",
                class: "grid grid-cols-1 gap-4",

                for t in all_titles(){
                    Link {
                        class: "block p-4 bg-blue-50 hover:bg-blue-100 dark:bg-blue-900 dark:hover:bg-blue-800 \
                                rounded-md text-blue-800 dark:text-blue-300 \
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
            class: "flex justify-between items-center mt-8 pt-4 border-t border-gray-200 dark:border-gray-700",

            // Previous Blog Link
            {
                if let Some(a) = prev {
                    rsx! {
                        Link {
                            to: Route::BlogView { title: a.clone()},
                            class: "px-6 py-3 bg-blue-500 text-white font-semibold rounded-lg \
                                    shadow-md hover:bg-blue-600 transition duration-300 ease-in-out \
                                    text-base sm:text-lg \
                                    dark:bg-blue-700 dark:hover:bg-blue-600",
                            "← {a}"
                        }
                    }
                } else {
                    rsx! {
                        div {
                            class: "px-6 py-3 bg-gray-300 text-gray-600 font-semibold rounded-lg \
                                    cursor-not-allowed text-base sm:text-lg \
                                    dark:bg-gray-700 dark:text-gray-400",
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
                                    text-base sm:text-lg \
                                    dark:bg-blue-700 dark:hover:bg-blue-600",
                            "{b} →"
                        }
                    }
                } else {
                    rsx! {
                        div {
                            class: "px-6 py-3 bg-gray-300 text-gray-600 font-semibold rounded-lg \
                                    cursor-not-allowed text-base sm:text-lg \
                                    dark:bg-gray-700 dark:text-gray-400",
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
