use dioxus::prelude::*;
use hot_dog::guide_state::DogView;

#[derive(Routable, Clone, PartialEq)]
pub enum Route {
    #[layout(NavBar)] // -> use the NavBar function
    #[route("/")]
    DogView,

    #[route("/favorites")]
    Favorites,
}

#[component]
pub fn Favorites() -> Element {
    rsx! { "favorites!" }
}

#[component]
pub fn NavBar() -> Element {
    rsx! {
        div { id: "title???",
            Link { to: Route::DogView,
                h1 { "🌭 HotDog! " }
            }
            Link { to: Route::Favorites, id: "heart", "♥️" } // <------- add this Link
        }
        Outlet::<Route> {}
    }
}

#[component]
pub fn app() -> Element {
    rsx! {
        document::Stylesheet { href: asset!("/assets/main.css") }
        Router::<Route> {}
    }
}
