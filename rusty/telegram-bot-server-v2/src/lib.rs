#![feature(box_into_inner)]
#![feature(associated_type_defaults)]
#![feature(box_syntax)]

pub mod app;

mod opts;
pub use opts::*;

pub mod deliver;
pub use deliver::{Deliver, Msg2Deliver};
