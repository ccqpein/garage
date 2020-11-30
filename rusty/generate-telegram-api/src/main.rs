use curl::easy::Easy;
use Crawler::parser_chain::{ParserSet, ParserTree};
use Crawler::tools::*;
use Crawler::{ElementRef, Html, Node};

fn get_telegram_html() -> Html {
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

    // make page in a huge string
    let page = String::from_utf8(dst).unwrap();

    Html::parse_fragment(&page)
}

fn main() {
    let fragment = get_telegram_html();
    let find_doc_parser = ParserTree::new(&[
        r#"div[class="dev_page_wrap"]"#,
        r#"div[class="container clearfix"]"#,
        r#"div[id="dev_page_content"]"#,
    ])
    .unwrap();

    //:= TODO: need pick all tags
    //:= TODO: then use children to check

    let mut doc = find_doc_parser.select_html(&fragment)[0];
    let mut left_children = skip_children_after(&mut doc, |c| match c.value() {
        Node::Element(x) => {
            if x.name() == "p" {
                if ElementRef::wrap(*c).unwrap().inner_html()
                    == r##"The majority of bots will be OK with the default configuration, running on our servers. But if you feel that you need one of <a href="#using-a-local-bot-api-server">these features</a>, you're welcome to switch to your own at any time."##
                {
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        _ => false,
    }).unwrap();

    let left_children = left_children.map(|nr| ElementRef::wrap(nr)).collect();

    let find_all_tags_parser = ParserSet::new(&[r#"h4"#, r#"h3"#, r#"p"#, r#"table"#]);
    find_all_tags_parser.select_ele()
}
