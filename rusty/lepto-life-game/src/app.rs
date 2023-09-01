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
    //let canva = leptos::html::canvas(cx);
    // let canva = Canvas::default();
    // canva.set_width(width);
    // canva.set_height(height);
    //println!("{:?}", canva.width());
    //println!("{:?}", canva.get_context("2d"));
    //println!("{:?}", &canva);

    let canvas_node = create_node_ref::<Canvas>(cx);
    // let ctx = canvas_node
    //     .get()
    //     .unwrap()
    //     .get_context("2d")
    //     .ok()
    //     .flatten()
    //     .expect("canvas to have context")
    //     .unchecked_into::<web_sys::CanvasRenderingContext2d>();

    canvas_node.on_load(cx, move |canvas_ref| {
        canvas_ref.on_mount(move |x| {
            // get access to the canvas
            let ctx = x
                .get_context("2d")
                .ok()
                .flatten()
                .expect("")
                .unchecked_into::<web_sys::CanvasRenderingContext2d>();
            // draw rectangle on the canvas at the position (x,y) and provide width and height
            ctx.fill_rect(10.0, 10.0, 10.0, 10.0);
        });
    });

    //canvas_node.on_load(cx, move |ctx| {});

    view! { cx,
            <p>hello</p>
            <canvas id="test" node_ref=canvas_node></canvas>
    }
}
