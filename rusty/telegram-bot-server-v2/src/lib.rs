#![feature(box_into_inner)]

mod app;
mod opts;
pub use opts::*;

pub mod deliver;
use deliver::{Deliver, Msg2Deliver};
