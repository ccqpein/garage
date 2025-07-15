use std::{cell::RefCell, rc::Rc};

use crate::home::*;

use dioxus::prelude::*;
use dioxus_router::prelude::*;

/// An enum of all of the possible routes in the app.
#[derive(Routable, Clone)]
pub enum Route {
    #[route("/")]
    Home {},

    /// for some reason, I had to write all the props here
    #[route("/:title")]
    BlogView { title: String },
}
