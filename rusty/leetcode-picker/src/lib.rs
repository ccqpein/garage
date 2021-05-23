use reqwest::blocking::{get, Response};
use scraper::{Html, Selector};

const QUESTION_ID_SELECTOR: &str = r#"div.question__1Nd3.active__iWA5"#;

/// return response
fn get_quiz(url: &str) -> Result<Response, String> {
    get(url).map_err(|e| e.to_string())
}

fn parse_response_body(rep: Response) {
    match rep.text() {
        Ok(ref s) => {
            let hh = Html::parse_fragment(s);

            //:= TODO: get header?
            //:= TODO: get question id
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

#[cfg(test)]
mod tests {
    use super::*;
    use scraper::{Html, Selector};

    #[test]
    fn selector_test() {
        let a = r#"
<div class="question__1Nd3 active__iWA5" data-title-slug="capacity-to-ship-packages-within-d-days" data-question-id="1056" data-paid-only="false" title="capacity-to-ship-packages-within-d-days"></div>
"#;
        let fragment = Html::parse_fragment(a);
        //let selector = Selector::parse(r#"div[class="question__1Nd3 active__iWA5"]"#).unwrap();
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
}
