use super::regex_handle::*;
use ego_tree::NodeRef;
use reqwest::blocking::{get, Response};
use scraper::{ElementRef, Html, Node, Selector};

const QUESTION_ID_SELECTOR: &str = r#"div.question__1Nd3.active__iWA5"#;
const QUESTION_DESCRIPTION: &str = r#"div.content__u3I1.question-content__JfgR"#;

pub fn parse_response_body(rep: Response) {
    match rep.text() {
        Ok(ref s) => {
            let hh = Html::parse_fragment(s);

            //:= TODO: get header?
            //:= TODO: get question id

            let q_des = description_nodes(&hh);
            description_markdown(q_des)
                .iter()
                .for_each(|s| println!("{}", s));
        }
        Err(_) => {} //:= todo
    }
}

fn find_question_id(h: &Html) -> Result<String, String> {
    let selector = Selector::parse(QUESTION_ID_SELECTOR).unwrap();
    let mut a = h.select(&selector);

    match a.find_map(|ele| ele.value().attrs().find(|(k, _)| *k == "data-question-id")) {
        Some((_, id)) => Ok(id.into()),
        None => Err("Cannot find id".into()),
    }
}

fn description_nodes(h: &Html) -> impl Iterator<Item = NodeRef<'_, Node>> {
    let selector = Selector::parse(QUESTION_DESCRIPTION).unwrap();
    let mut a = h.select(&selector);

    a.next()
        .unwrap()
        .first_child() // <- div
        .unwrap()
        .children()
}

fn description_markdown<'a>(ir: impl Iterator<Item = NodeRef<'a, Node>>) -> Vec<String> {
    ir.filter_map(|n| match n.value() {
        Node::Text(s) => Some(s.to_string()),
        Node::Element(e) => match e.name() {
            "p" => Some(ElementRef::wrap(n).unwrap().inner_html()),
            "pre" => Some(ElementRef::wrap(n).unwrap().html()),
            "ul" => Some(ElementRef::wrap(n).unwrap().inner_html()),
            _ => None,
        },
        _ => None,
    })
    .map(|mut chunk| {
        clean_all_tags(&mut chunk);
        chunk
    })
    .collect::<Vec<String>>()
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Write;

    use super::*;
    use scraper::{ElementRef, Html, Selector};

    #[test]
    fn selector_test() {
        let a = r#"
<div class="question__1Nd3 active__iWA5" data-title-slug="capacity-to-ship-packages-within-d-days" data-question-id="1056" data-paid-only="false" title="capacity-to-ship-packages-within-d-days"></div>
"#;
        let fragment = Html::parse_fragment(a);

        let selector0 = Selector::parse(r#"div.question__1Nd3.active__iWA5"#).unwrap();

        for element in fragment.select(&selector0) {
            assert_eq!(
                element
                    .value()
                    .attrs()
                    .find(|(k, _)| *k == "data-question-id")
                    .unwrap()
                    .1,
                "1056"
            );
        }
    }

    #[test]
    fn test_find_question_id() {
        let a = r#"
<div class="question__1Nd3 active__iWA5" data-title-slug="capacity-to-ship-packages-within-d-days" data-question-id="1056" data-paid-only="false" title="capacity-to-ship-packages-within-d-days"></div>
"#;
        let fragment = Html::parse_fragment(a);
        assert_eq!(find_question_id(&fragment).unwrap(), "1056".to_string());
    }

    #[test]
    fn test_find_description() {
        let a = include_str!("../tests/questions_description_test_case0");
        let fragment = Html::parse_fragment(a);

        //description_nodes(&fragment).for_each(|ele| println!("v: {:?}", ele.value()));

        let mut vv = description_nodes(&fragment);
        assert_eq!(ElementRef::wrap(vv.next().unwrap()).unwrap().html(),"<p>A conveyor belt has packages that must be shipped from one port to another within <code>days</code> days.</p>".to_string());
        vv.next(); // <- kill Text("\n\n")
        assert_eq!(ElementRef::wrap(vv.next().unwrap()).unwrap().inner_html(),"The <code>i<sup>th</sup></code> package on the conveyor belt has a weight of <code>weights[i]</code>. Each day, we load the ship with packages on the conveyor belt (in the order given by <code>weights</code>). We may not load more weight than the maximum weight capacity of the ship.".to_string());
    }

    #[test]
    fn test_description_markdown() {
        let a = include_str!("../tests/questions_description_test_case0");
        let fragment = Html::parse_fragment(a);
        let content = description_markdown(description_nodes(&fragment));
        dbg!(&content);

        let mut file = File::create("./tests/questions_description_test_case0.md").unwrap();
        for c in content {
            file.write(&c.as_bytes()).unwrap();
        }
    }
}
