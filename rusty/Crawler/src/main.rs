use curl::easy::Easy;
use scraper::node::Node;
use scraper::ElementRef;
use scraper::{Html, Selector};
use std::io::{stdout, Write};
use Crawler::parser_chain::{ParserSet, ParserTree};
use Crawler::tools::*;

fn main() {
    let page = r#"<div id="dev_page_content">
<div>
<h3>aa</h3>
<p>pp</p>
<h2>h2</h2>
<h4>"The majority of bots will be OK with the default configuration, running on our servers. But if you feel that you need one of" <a>aa</a> ", you're welcome to switch to your own at any time."</h4>
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
    tree.select_html(&fragment).iter().for_each(|s| {
        println!("tree & element ref: {:?}", s.text().collect::<Vec<_>>());
    });

    let set = ParserSet::new(&["p", "h2", "h3", "table", "div"]).unwrap();
    // set.select_ele(&node)
    //     .iter()
    //     .for_each(|s| println!("{:?}", s.text().collect::<Vec<_>>()));

    let node = tree.select_html(&fragment); // second level div
    set.select_ele(&node[0])
        .iter()
        .for_each(|s| println!("SET: {:?}", s.value()));

    // for child in tree.select_html(&fragment)[0].children() {
    //     match child.value() {
    //         Node::Element(x) => println!("element:{:?}", x.name()),
    //         _ => println!("{:?}", child.value()),
    //     }
    // }

    let mut noderef = tree.select_html(&fragment)[0];

    let mut children = skip_children_after(&mut noderef, |c| match c.value() {
        Node::Element(x) => {
            if x.name() == "h4" {
                println!("{:?}", ElementRef::wrap(*c).unwrap().inner_html());
                true
            } else {
                false
            }
        }
        _ => false,
    })
    .unwrap();
    println!("next is: {:?}", children.next().unwrap().value());
}
