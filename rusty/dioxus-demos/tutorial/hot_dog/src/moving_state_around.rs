/// this file should learn/review with the src/reactivity.rs
use dioxus::prelude::*;

// method 1 is pass signal directly

// pub fn ParentComponent() -> Element {
//     let count = use_signal(|| 0);

//     rsx! {
//         "Count is {count}"
//         IncrementButton {
//             count
//         }
//     }
// }

// #[component]
// fn IncrementButton(mut count: Signal<i32>) -> Element {
//     rsx! {
//         button {
//             onclick: move |_| count += 1,
//             "Increment"
//         }
//     }
// }

// method 2 use context, means I don't need to pass the context explicit
// emmmmmmm, I dont like it

// #[derive(Clone, Copy)]
// struct MyState {
//     count: Signal<i32>,
// }

// pub fn ParentComponent() -> Element {
//     // Use context provider provides an unique type to all children of this component
//     let state = use_context_provider(|| MyState {
//         count: Signal::new(0),
//     });

//     rsx! {
//         "Count is {state.count}"
//         // IncrementButton will have access to the count without explicitly passing it through props
//         IncrementButton {}
//     }
// }

// #[component]
// fn IncrementButton() -> Element {
//     // Use context gets the value from a parent component
//     let mut count = use_context::<MyState>().count;

//     rsx! {
//         button {
//             onclick: move |_| count += 1,
//             "Increment"
//         }
//     }
// }

// method 3 globals, I also don't like it.

// Globals are created the first time you access them with the closure you pass to Global::new
static COUNT: GlobalSignal<i32> = Global::new(|| 0);

pub fn ParentComponent() -> Element {
    rsx! {
        "Count is {COUNT}"
        IncrementButton {}
    }
}

fn IncrementButton() -> Element {
    rsx! {
        button {
            // You don't need to pass anything around or get anything out of the context because COUNT is global
            onclick: move |_| *COUNT.write() += 1,
            "Increment"
        }
    }
}
