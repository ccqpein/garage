#![feature(box_into_inner)]

pub mod app;

mod opts;
pub use opts::*;

pub mod deliver;
pub use deliver::{Deliver, Msg2Deliver};
