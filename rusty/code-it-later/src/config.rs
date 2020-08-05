use clap::Clap;
use std::{env, ffi::OsString};

#[derive(Default)]
pub struct Config {
    pub(super) filetypes: Vec<OsString>,
    pub(super) ignore_dirs: Vec<OsString>,
    keywords: Option<Vec<String>>,
    dirs: Option<Vec<String>>,
}

impl From<&Args> for Config {
    fn from(a: &Args) -> Self {
        Self {
            filetypes: a.filetypes.clone(),
            ignore_dirs: a.ignore_dir.clone(),
            keywords: a.keywords.clone(),
            dirs: a.dirs.clone(),
        }
    }
}

/// Args struct from command line
#[derive(Default, Clap, Debug)]
#[clap(version = "0.1.0", author = "ccQpein")]
pub struct Args {
    #[clap(short, long)]
    filetypes: Vec<OsString>,

    #[clap(short, long, default_value = ".")]
    dirs: Option<Vec<String>>,

    #[clap(short = "x", long = "ignore-dir")]
    ignore_dir: Vec<OsString>,

    #[clap(short, long)]
    keywords: Option<Vec<String>>,

    #[clap(short, long)]
    jsonx: Option<String>,
}
