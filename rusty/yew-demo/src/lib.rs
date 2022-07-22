// use wasm_bindgen::prelude::*;
// use yew::prelude::*;

// struct Model {
//     link: ComponentLink<Self>,
//     value: i64,
// }

// enum Msg {
//     AddOne,
// }

// impl Component for Model {
//     type Message = Msg;
//     type Properties = ();
//     fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
//         Self { link, value: 0 }
//     }

//     fn update(&mut self, msg: Self::Message) -> ShouldRender {
//         match msg {
//             Msg::AddOne => self.value += 1,
//         }
//         true
//     }

//     fn change(&mut self, _props: Self::Properties) -> ShouldRender {
//         // Should only return "true" if new properties are different to
//         // previously received properties.
//         // This component has no properties so we will always return "false".
//         false
//     }

//     fn view(&self) -> Html {
//         html! {
//             <div>
//                 <button onclick=self.link.callback(|_| Msg::AddOne)>{ "+1" }</button>
//                 <p>{ self.value }</p>
//             </div>
//         }
//     }
// }

// #[wasm_bindgen(start)]
// pub fn run_app() {
//     App::<Model>::new().mount_to_body();
// }

mod components_example;

use yew::html::Context;
use yew::*;

pub enum Msg {
    AddOne,
}

pub struct Model {
    value: i64,
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self { value: 0 }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::AddOne => {
                self.value += 1;
                // the value has changed so we need to
                // re-render for it to appear on the page
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        // This gives us a component's "`Scope`" which allows us to send messages, etc to the component.
        let link = ctx.link();
        html! {
            <div>
                <button onclick={link.callback(|_| Msg::AddOne)}>{ "add one" }</button>
                <p>{ self.value }</p>
            </div>
        }
    }
}
