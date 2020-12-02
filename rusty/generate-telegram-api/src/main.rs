use cssparser::ParseError;
use curl::easy::Easy;
use scraper::{ElementRef, Html, Selector};
use selectors::parser::SelectorParseErrorKind;

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

fn find_doc_real_body<'a>(
    tags: &'a [&str],
    html: &'a Html,
) -> Result<ElementRef<'a>, ParseError<'a, SelectorParseErrorKind<'a>>> {
    let mut element_ref = vec![html.root_element()];
    for tag in tags {
        let selector = match Selector::parse(tag) {
            Ok(h) => h,
            Err(e) => return Err(e),
        };

        element_ref = element_ref
            .iter()
            .map(|ele| ele.select(&selector).collect::<Vec<ElementRef<'a>>>())
            .flatten()
            .collect();
    }

    Ok(element_ref[0])
}

fn pick_all_informations_with_filter<'a, P>(
    tags: &'a [&str],
    ele: &ElementRef<'a>,
    predicate: &mut P,
) -> Vec<ElementRef<'a>>
where
    P: FnMut(&ElementRef<'a>) -> bool,
{
    let selectors: Vec<Selector> = tags.iter().map(|s| Selector::parse(s).unwrap()).collect();
    let mut result = vec![];
    for e in ele.children() {
        if let Some(a) = ElementRef::wrap(e) {
            if selectors.iter().any(|sele| sele.matches(&a)) && predicate(&a) {
                result.push(a)
            }
        } else {
            //println!("not element: {:?}", e.value());
        }
    }
    result
}

fn main() {
    let fragment = get_telegram_html();
    let find_doc_parser = vec![
        r#"div[class="dev_page_wrap"]"#,
        r#"div[class="container clearfix"]"#,
        r#"div[id="dev_page_content"]"#,
    ];

    let body = find_doc_real_body(&find_doc_parser, &fragment).unwrap();
    //println!("{:?}", body.inner_html());
    let set = vec!["p", "h4", "h3", "table", "ol"];
    let mut started = false;
    let mut pridicate = |s: &ElementRef| {
        if started == true {
            return true;
        };
        let x = s.value();

        if x.name() == "p" {
            if s.inner_html()
                == r##"The majority of bots will be OK with the default configuration, running on our servers. But if you feel that you need one of <a href="#using-a-local-bot-api-server">these features</a>, you're welcome to switch to your own at any time."##
            {
                started = true;
                return true;
            }
        }

        false
    };

    let data = pick_all_informations_with_filter(&set, &body, &mut pridicate);

    for i in 0..2 {
        println!("{:?}", data[i].inner_html());
    }

    //:= TODO: need pick all tags
    //:= TODO: then use children to check

    // let mut doc = find_doc_parser.select_html(&fragment)[0];
    // let mut left_children = skip_children_after(&mut doc, |c| match c.value() {
    //     Node::Element(x) => {
    //         if x.name() == "p" {
    //             if ElementRef::wrap(*c).unwrap().inner_html()
    //                 == r##"The majority of bots will be OK with the default configuration, running on our servers. But if you feel that you need one of <a href="#using-a-local-bot-api-server">these features</a>, you're welcome to switch to your own at any time."##
    //             {
    //                 true
    //             } else {
    //                 false
    //             }
    //         } else {
    //             false
    //         }
    //     }
    //     _ => false,
    // }).unwrap();

    // let left_children = left_children.map(|nr| ElementRef::wrap(nr)).collect();

    // let find_all_tags_parser = ParserSet::new(&[r#"h4"#, r#"h3"#, r#"p"#, r#"table"#]);
    // find_all_tags_parser.select_ele()
}
