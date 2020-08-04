use clap::Clap;
use std::env;

pub struct FsOpConfig {
    filetypes: Option<Vec<String>>,
    keywords: Option<Vec<String>>,
    dirs: Option<Vec<String>>,
}

/// argvs struct from command line
#[derive(Default, Clap, Debug)]
#[clap(version = "0.1.0", author = "ccQpein")]
pub struct Argvs {
    #[clap(short, long)]
    filetypes: Vec<String>,

    #[clap(short, long, default_value = ".")]
    dirs: Vec<String>,

    #[clap(short = "dx", long = "ignore-dir")]
    ignore_dir: Vec<String>,

    #[clap(short, long)]
    keywords: Vec<String>,

    #[clap(short, long)]
    jsonx: String,
}

impl Argvs {
    fn new() -> Self {
        let mut argvs = env::args();
        let mut a: Self = Default::default();
        while let Some(x) = argvs.next() {
            match x.as_str() {
                "-f" | "--filetype" => {}
                "-k" | "--keyword" => {}
                "-d" | "--dir" => {}
                "-j" | "--jsonx" => {}
                "-dx" | "--ignore-dir" => {}
                _ => {}
            }
        }

        a
    }
}
