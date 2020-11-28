use curl::easy::Easy;
use Crawler::parser_chain::{ParserSet, ParserTree};
use Crawler::Html;

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

    let mut doc = find_doc_parser.select_html(&fragment)[0].children();
    for i in 0..9 {
        println!("{:?}", doc.next().unwrap().value())
    }
    let find_all_tags_parser = ParserSet::new(&[r#"h4"#, r#"h3"#, r#"p"#]);
}
