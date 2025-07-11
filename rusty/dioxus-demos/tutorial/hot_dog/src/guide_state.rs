use dioxus::prelude::*;

static CSS: Asset = asset!("/assets/main.css");

// #[component]
// fn App() -> Element {
//     rsx! {
//         document::Stylesheet { href: CSS }
//         Title {}
//         DogView {}
//     }
// }

// #[component]
// fn Title() -> Element {
//     rsx! {
//         div { id: "title",
//             h1 { "HotDog! 🌭" }
//         }
//     }
// }

// Create a new wrapper type
#[derive(Clone)]
struct TitleState(String);

fn App() -> Element {
    // Provide that type as a Context
    use_context_provider(|| TitleState("HotDog".to_string()));
    rsx! {
        Title {}
    }
}

fn Title() -> Element {
    // Consume that type as a Context
    let title = use_context::<TitleState>();
    rsx! {
        h1 { "{title.0}" }
    }
}

// #[component]
// fn DogView() -> Element {
//     rsx! {
//         div { id: "dogview",
//             img { src: "https://images.dog.ceo/breeds/pitbull/dog-3981540_1280.jpg" }
//         }
//         div { id: "buttons",
//             button { id: "skip", "skip" }
//             button { id: "save", "save!" }
//         }
//     }
// }

// #[component]
// pub fn DogView() -> Element {
//     let skip = move |evt| {};
//     let save = move |evt| {};
//     rsx! {
//         div { id: "dogview",
//             img { src: "https://images.dog.ceo/breeds/pitbull/dog-3981540_1280.jpg" }
//         }
//         div { id: "buttons",
//             button { onclick: skip, id: "skip",  "skip" }
//             button { onclick: save, id: "save",  "save!" }
//         }
//     }
// }

#[derive(serde::Deserialize)]
struct DogApi {
    message: String,
}

#[component]
pub fn DogView() -> Element {
    // let mut img_src = use_signal(|| "".to_string());

    // let fetch_new = move |_| async move {
    //     let response = reqwest::get("https://dog.ceo/api/breeds/image/random")
    //         .await
    //         .unwrap()
    //         .json::<DogApi>()
    //         .await
    //         .unwrap();

    //     img_src.set(response.message);
    // };

    // let skip = move |evt| {};
    // let save = move |evt| {};
    // let img_src = use_hook(|| "https://images.dog.ceo/breeds/pitbull/dog-3981540_1280.jpg");

    let mut img_src = use_resource(|| async move {
        reqwest::get("https://dog.ceo/api/breeds/image/random")
            .await
            .unwrap()
            .json::<DogApi>()
            .await
            .unwrap()
            .message
    });
    rsx! {
        div { id: "dogview",
              //img { src: "{img_src}" }
              img { src: img_src.cloned().unwrap_or_default() }
        }
        div { id: "buttons",
              button { onclick:  move |_| img_src.restart(), id: "skip",  "skip" }
              button { onclick:  move |_| img_src.restart(), id: "save",  "save!" }
        }
    }
}

static SONG: GlobalSignal<String> = Signal::global(|| "Drift Away".to_string());

#[component]
fn Player() -> Element {
    rsx! {
        h3 { "Now playing {SONG}" }
        button {
            onclick: move |_| *SONG.write() = "Vienna".to_string(),
            "Shuffle"
        }
    }
}
