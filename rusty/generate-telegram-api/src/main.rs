use cssparser::ParseError;
use curl::easy::Easy;
use scraper::node::Node;
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

///
///
#[derive(Debug)]
enum Status {
    Nil,

    DataTypeName,
    DataTypeDoc,
    DataTypeTable,

    MethodName,
    MethodDoc,
    MethodTable,
}
impl Status {
    fn is_table(&self) -> bool {
        match self {
            Self::DataTypeTable | Self::MethodTable => true,
            _ => false,
        }
    }
}

fn is_datatype(s: &str) -> bool {
    if let Some(c) = s.chars().next() {
        c.is_uppercase()
    } else {
        false
    }
}

fn is_method(s: &str) -> bool {
    if let Some(c) = s.chars().next() {
        c.is_lowercase()
    } else {
        false
    }
}

fn h4_check(e: &ElementRef, status: &mut Status) {
    if let Some(node) = e.last_child() {
        match node.value() {
            Node::Text(te) => {
                if is_datatype(&te) {
                    *status = Status::DataTypeName;
                    println!("h4: {:?}", &te);
                } else if is_method(&te) {
                    *status = Status::MethodName;
                }
            }
            _ => {}
        };
    }
}

fn p_check(e: &ElementRef, status: &mut Status) {
    match status {
        Status::DataTypeName => *status = Status::DataTypeDoc,
        Status::MethodName => *status = Status::MethodDoc,
        _ => (),
    }
}

fn table_check(e: &ElementRef, status: &mut Status) {
    match status {
        Status::MethodName | Status::MethodDoc => *status = Status::MethodTable,
        Status::DataTypeName | Status::DataTypeDoc => *status = Status::DataTypeTable,
        _ => (),
    }
}

fn generate_structs<'a>(ele_l: Vec<ElementRef<'a>>) {
    let mut status = Status::Nil;
    for ele in ele_l {
        match ele.value().name() {
            "h4" => h4_check(&ele, &mut status),
            "p" => p_check(&ele, &mut status),
            "table" => table_check(&ele, &mut status),
            _ => {}
        }

        //:= TODO: generate struct

        if status.is_table() {
            status = Status::Nil;
        }
    }
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

    for i in 0..10 {
        //println!("{:?}", data[i].html());
    }

    let mut status = Status::Nil;
    h4_check(&data[4], &mut status);
    println!("{:?}", status);
}
