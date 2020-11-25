use curl::easy::Easy;
use scraper::{Html, Selector};
use std::io::{stdout, Write};
use Crawler::parser_chain::{ParserSet, ParserTree};

fn main() {
    /// example
    // let mut dst = Vec::new();
    // let mut easy = Easy::new();
    // easy.url("https://core.telegram.org/bots/api").unwrap();

    // let mut transfer = easy.transfer();

    // transfer
    //     .write_function(|data| {
    //         dst.extend_from_slice(data);
    //         Ok(data.len())
    //     })
    //     .unwrap();

    // transfer.perform().unwrap();
    // drop(transfer); // Drop!

    // // make page in a huge string
    // let page = String::from_utf8(dst).unwrap();
    let page = r#"<div id="dev_page_content">
<div>
<h3>aa</h3>
<p>pp</p>
<h2>h2</h2>
<h3>Getting updates</h3>
<table></table>
<div><h4>h4</h4></div>
</div>
</div>"#;
    let fragment = Html::parse_fragment(&page);

    for ff in fragment.root_element().children() {
        println!("{:?}", ff.value());
    }

    let tree = ParserTree::new(&[r#"div[id="dev_page_content"]"#, r#"h3"#]).unwrap();
    tree.select_html(&fragment).iter().for_each(|s| {
        println!("tree & html: {:?}", s.text().collect::<Vec<_>>());
    });

    /// next test
    let tree = ParserTree::new(&[r#"div[id="dev_page_content"]"#, r#"div"#]).unwrap();
    let node = tree.select_html(&fragment).iter().for_each(|s| {
        println!("tree & element ref: {:?}", s.text().collect::<Vec<_>>());
    });

    let set = ParserSet::new(&["p", "h2", "h3", "table", "div"]).unwrap();
    // set.select_ele(&node)
    //     .iter()
    //     .for_each(|s| println!("{:?}", s.text().collect::<Vec<_>>()));

    set.select_html(&fragment)
        .iter()
        .for_each(|s| println!("{:?}", s.text().collect::<Vec<_>>()));
}
