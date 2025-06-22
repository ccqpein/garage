// fn DogApp(props: DogAppProps) -> Element {
//     todo!()
// }

// #[derive(Props, PartialEq, Clone)]
// struct DogAppProps {
//     breed: String,
// }

// this look like as same as upper
// just use the component macro

use dioxus::prelude::*;

#[component]
pub fn DogApp(breed: String) -> Element {
    rsx! {
        "Breed: {breed}"
    }
}

#[component]
fn App() -> Element {
    rsx! {
        DogApp { breed: "corgi" }
    }
}
