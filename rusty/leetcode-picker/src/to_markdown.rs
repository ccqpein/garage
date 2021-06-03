use super::regex_handle::*;
use ego_tree::NodeRef;
use reqwest::blocking::Response;
use scraper::{ElementRef, Html, Node, Selector};

pub(super) fn graphql_response_parse(rep: Response) -> Result<serde_json::Value, String> {
    match rep.text() {
        Ok(c) => {
            //dbg!(&c);
            serde_json::from_str(&c).map_err(|e| e.to_string())
        }
        Err(e) => Err(e.to_string()),
    }
}

pub(super) fn find_question_id_from_graphql_req(obj: &serde_json::Value) -> Result<String, String> {
    match obj.get("data") {
        Some(d) => match d.get("question") {
            Some(q) => match q.get("questionFrontendId") {
                Some(id) => id.as_str().map(|x| x.into()).ok_or("Not Found".into()),
                None => Err("Not Found".into()),
            },
            None => Err("Not Found".into()),
        },
        None => Err("Not Found".into()),
    }
}

pub(super) fn find_question_level_from_graphql_req(
    obj: &serde_json::Value,
) -> Result<String, String> {
    match obj.get("data") {
        Some(d) => match d.get("question") {
            Some(q) => match q.get("difficulty") {
                Some(id) => id.as_str().map(|x| x.into()).ok_or("Not Found".into()),
                None => Err("Not Found".into()),
            },
            None => Err("Not Found".into()),
        },
        None => Err("Not Found".into()),
    }
}

pub(super) fn find_question_title_from_graphql_req(
    obj: &serde_json::Value,
) -> Result<String, String> {
    match obj.get("data") {
        Some(d) => match d.get("question") {
            Some(q) => match q.get("title") {
                Some(id) => id.as_str().map(|x| x.into()).ok_or("Not Found".into()),
                None => Err("Not Found".into()),
            },
            None => Err("Not Found".into()),
        },
        None => Err("Not Found".into()),
    }
}

pub(super) fn find_question_content(obj: &serde_json::Value) -> Result<&str, String> {
    match obj.get("data") {
        Some(d) => match d.get("question") {
            Some(q) => match q.get("content") {
                Some(id) => id.as_str().ok_or("Not Found".into()),
                None => Err("Not Found".into()),
            },
            None => Err("Not Found".into()),
        },
        None => Err("Not Found".into()),
    }
}

pub(super) fn description_in_graphql(h: &Html) -> impl Iterator<Item = NodeRef<'_, Node>> {
    h.root_element().children()
}

pub(super) fn description_markdown<'a>(ir: impl Iterator<Item = NodeRef<'a, Node>>) -> Vec<String> {
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
    use super::*;
    use scraper::{ElementRef, Html};
    use std::fs::File;
    use std::io::Write;

    #[test]
    fn test_find_description_from_graphql() {
        // content from graphql
        let a = include_str!("../tests/questions_description_test_case1");
        let fragment = Html::parse_fragment(a);

        let mut vv = description_in_graphql(&fragment);
        assert_eq!(ElementRef::wrap(vv.next().unwrap()).unwrap().html(),"<p>A conveyor belt has packages that must be shipped from one port to another within <code>days</code> days.</p>".to_string());
    }

    #[test]
    fn test_description_markdown() {
        // test graphql content
        let a = include_str!("../tests/questions_description_test_case1");
        let fragment = Html::parse_fragment(a);
        let content = description_markdown(description_in_graphql(&fragment));
        //dbg!(&content);

        let mut file = File::create("./tests/questions_description_test_case1.md").unwrap();
        for c in content {
            file.write(&c.as_bytes()).unwrap();
        }
    }
}
