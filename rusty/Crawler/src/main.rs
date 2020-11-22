use curl::easy::Easy;
use scraper::{Html, Selector};
use std::io::{stdout, Write};
use Crawler::parser_chain::ParserTree;

fn main() {
    /// example
    let mut dst = Vec::new();
    let mut easy = Easy::new();
    easy.url("https://core.telegram.org/bots/api").unwrap();

    let mut transfer = easy.transfer();

    transfer
        .write_function(|data| {
            dst.extend_from_slice(data);
            Ok(data.len())
        })
        .unwrap();

    transfer.perform().unwrap();
    drop(transfer); // Drop!

    let page = String::from_utf8(dst).unwrap();

    let fragment = Html::parse_fragment(&page);

    let tree = ParserTree::new(&[r#"div[id="dev_page_content"]"#, r#"h3"#]).unwrap();
    tree.select_html(&fragment).iter().for_each(|s| {
        if s.text().collect::<Vec<_>>()[0] == "Getting updates" {
            println!("oh yeah");
        }
    });
    // let selector0 = Selector::parse(r#"div[id="dev_page_content"]"#).unwrap();
    // let selector1 = Selector::parse(r#"h3"#).unwrap();

    // let ast0 = fragment.select(&selector0).next().unwrap();
    // for entry in ast0.select(&selector1) {
    //     println!("{:?}", entry.text().collect::<Vec<_>>());
    //     if entry.text().collect::<Vec<_>>()[0] == "Getting updates" {
    //         println!("oh yeah");
    //         return;
    //     }
    // }
    // println!("oh no");
}
