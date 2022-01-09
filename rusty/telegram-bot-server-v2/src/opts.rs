use clap::Parser;

#[derive(Default, Parser, Clone)]
#[clap(about, version, author)]
pub struct Opts {
    #[clap(short, long)]
    pub vault: String,
}
