use std::time::Duration;

use crate::snake::*;
use leptos::{html::Canvas, *};
use leptos_meta::*;
use leptos_router::*;

use wasm_bindgen::JsCast;

#[component]
pub fn App(cx: Scope) -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context(cx);

    view! { cx,
        // injects a stylesheet into the document <head>
        // id=leptos means cargo-leptos will hot-reload this stylesheet
        <Stylesheet id="leptos" href="/pkg/leptos_start.css"/>

        // sets the document title
        <Title text="Welcome to Leptos"/>

        // content for this welcome page
        <Router>
            <main>
                <Routes>
                    <Route path="" view=HomePage/>
                    <Route path="/*any" view=NotFound/>
                </Routes>
            </main>
        </Router>
    }
}

/// Renders the home page of your application.
#[component]
fn HomePage(cx: Scope) -> impl IntoView {
    // Creates a reactive value to update the button
    let (count, set_count) = create_signal(cx, 0);
    let on_click = move |_| set_count.update(|count| *count += 1);

    view! { cx,
        <h1>"Welcome to Leptos!"</h1>
        <button on:click=on_click>"Click Me: " {count}</button>
            <LifeGame width=30 height=30/>
            <SnakeGame width=30 height=30/>
    }
}

/// 404 - Not Found
#[component]
fn NotFound(cx: Scope) -> impl IntoView {
    // set an HTTP status code 404
    // this is feature gated because it can only be done during
    // initial server-side rendering
    // if you navigate to the 404 page subsequently, the status
    // code will not be set because there is not a new HTTP request
    // to the server
    #[cfg(feature = "ssr")]
    {
        // this can be done inline because it's synchronous
        // if it were async, we'd use a server function
        let resp = expect_context::<leptos_actix::ResponseOptions>(cx);
        resp.set_status(actix_web::http::StatusCode::NOT_FOUND);
    }

    view! { cx, <h1>"Not Found"</h1> }
}

/// life game
#[component]
fn LifeGame(cx: Scope, width: u32, height: u32) -> impl IntoView {
    let canvas_node = create_node_ref::<Canvas>(cx);

    canvas_node.on_load(cx, move |canvas_ref| {
        canvas_ref.set_width(width * 20);
        canvas_ref.set_height(height * 20);
        canvas_ref.on_mount(move |x| {
            // get access to the canvas
            let ctx = x
                .get_context("2d")
                .ok()
                .flatten()
                .expect("")
                .unchecked_into::<web_sys::CanvasRenderingContext2d>();
            ctx.set_fill_style(&"#CCCCCC".into());

            // draw rectangle on the canvas at the position (x,y) and provide width and height
            make_board(&ctx, width, height);
        });
    });

    //:= doesn't work
    // set_interval(
    //     move || {
    //         let ctx = canvas_node
    //             .get()
    //             .unwrap()
    //             .get_context("2d")
    //             .ok()
    //             .flatten()
    //             .expect("")
    //             .unchecked_into::<web_sys::CanvasRenderingContext2d>();
    //         make_board(&ctx, width, height);
    //     },
    //     Duration::from_secs(1),
    // );

    use_interval(cx, 1000, move || {
        //console_log("hello");
        let ctx = canvas_node
            .get()
            .unwrap()
            .get_context("2d")
            .ok()
            .flatten()
            .expect("")
            .unchecked_into::<web_sys::CanvasRenderingContext2d>();
        ctx.set_fill_style(&"#000000".into());
        make_board(&ctx, 30, 30);
    });

    view! { cx,
            <p>hello</p>
            <canvas id="test" node_ref=canvas_node></canvas>
    }
}

fn life_game_new(cx: Scope, width: u32, height: u32) -> (impl IntoView, NodeRef<Canvas>) {
    let canvas_node = create_node_ref::<Canvas>(cx);

    canvas_node.on_load(cx, move |canvas_ref| {
        canvas_ref.set_width(width * 20);
        canvas_ref.set_height(height * 20);
        canvas_ref.on_mount(move |x| {
            // get access to the canvas
            let ctx = x
                .get_context("2d")
                .ok()
                .flatten()
                .expect("")
                .unchecked_into::<web_sys::CanvasRenderingContext2d>();
            ctx.set_fill_style(&"#CCCCCC".into());

            // draw rectangle on the canvas at the position (x,y) and provide width and height
            make_board(&ctx, width, height);
        });
    });

    use_interval(cx, 1000, move || {
        //console_log("hello");
        let ctx = canvas_node
            .get()
            .unwrap()
            .get_context("2d")
            .ok()
            .flatten()
            .expect("")
            .unchecked_into::<web_sys::CanvasRenderingContext2d>();
        ctx.set_fill_style(&"#000000".into());
        make_board(&ctx, 30, 30);
    });

    (
        view! { cx,
                <p>hello</p>
                <canvas id="test" node_ref=canvas_node></canvas>
        },
        canvas_node,
    )
}

fn make_board(b: &web_sys::CanvasRenderingContext2d, row: u32, col: u32) {
    for r in 0..row {
        for c in 0..col {
            b.fill_rect(20.0 * c as f64, 20.0 * r as f64, 19.0, 19.0);
            //b.clear_rect(10.0 * c as f64, 10.0 * r as f64, 9.0, 9.0);
        }
    }
}

pub fn use_interval<F>(cx: Scope, interval_millis: u64, f: F)
where
    F: Fn() + Clone + 'static,
{
    create_effect(cx, move |prev_handle: Option<IntervalHandle>| {
        // effects get their previous return value as an argument
        // each time the effect runs, it will return the interval handle
        // so if we have a previous one, we cancel it
        if let Some(prev_handle) = prev_handle {
            prev_handle.clear();
        };

        // here, we return the handle
        set_interval_with_handle(
            f.clone(),
            // this is the only reactive access, so this effect will only
            // re-run when the interval changes
            Duration::from_millis(interval_millis),
        )
        .expect("could not create interval")
    });
}
