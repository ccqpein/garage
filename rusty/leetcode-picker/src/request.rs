use lazy_static::*;
use reqwest::blocking::Client;
use reqwest::blocking::Request;
use reqwest::cookie::Jar;
use reqwest::IntoUrl;
use reqwest::Method;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;

use super::quiz::{Level, Quiz};

const LC_GRAPHQL_ENDPOINT: &str = "https://leetcode.com/graphql";
const LC_GRAPHQL_BODY: [&str; 2] = [
    r#"{"operationName": "questionData", "variables": {"titleSlug": ""#,
    r#""},"query": "query questionData($titleSlug: String!) {question(titleSlug: $titleSlug) {questionId questionFrontendId boundTopicId title titleSlug content translatedTitle translatedContent isPaidOnly difficulty likes dislikes isLiked similarQuestions exampleTestcases contributors {username profileUrl avatarUrl __typename} topicTags {name slug translatedName __typename} companyTagStats codeSnippets {lang langSlug code __typename} stats hints solution {id canSeeDetail paidOnly hasVideoSolution paidOnlyVideo __typename} status sampleTestCase metaData judgerAvailable judgeType mysqlSchemas enableRunCode enableTestMode enableDebugger envInfo libraryUrl adminUrl __typename}}"}"#,
];

/// referer header for all quiz & random-one-question
const LC_ALL_QUIZ_REFERER: &str = "https://leetcode.com/problemset/all/";

/// endpoint of all quiz
const LC_ALL_QUIZ_API: &str = "https://leetcode.com/api/problems/all/";

/// endpoint of random pick
const LC_RANDOM_QUIZ_API: &str = "https://leetcode.com/problems/random-one-question/all";

lazy_static! {
    /// csrf token
    static ref CSRFTOKEN: Mutex<Option<String>> = Mutex::new(None);
}

/// return response
/// url will return bunch of js script, need name in url to make json request
pub(super) fn get_quiz_by_url(url: &str) -> Result<Quiz, String> {
    let token = get_csrftoken("./vault/csrftoken")?; //:= need change to some other path
    let cli = make_client(&token, LC_GRAPHQL_ENDPOINT).map_err(|e| e.to_string())?;
    let req = request_builder(
        &cli,
        Method::POST,
        LC_GRAPHQL_ENDPOINT,
        url,
        &token,
        LC_GRAPHQL_BODY[0].to_string() + url.rsplit('/').nth(1).unwrap() + LC_GRAPHQL_BODY[1],
    )
    .map_err(|e| e.to_string())?;

    let resp = cli.execute(req).map_err(|e| e.to_string())?;
    Quiz::from_resp(resp)
}

pub(super) fn get_all_quiz() -> Result<serde_json::Value, String> {
    let token = get_csrftoken("./vault/csrftoken")?; //:= need change to some other path
    let cli = make_client(&token, LC_ALL_QUIZ_API).map_err(|e| e.to_string())?;
    let req = request_builder(
        &cli,
        Method::GET,
        LC_ALL_QUIZ_API,
        LC_ALL_QUIZ_REFERER,
        &token,
        String::new(),
    )
    .map_err(|e| e.to_string())?;

    match cli.execute(req).map_err(|e| e.to_string())?.text() {
        Ok(c) => serde_json::from_str(&c).map_err(|e| e.to_string()),
        Err(e) => Err(e.to_string()),
    }
}

pub(super) fn get_quiz_by_id(id: u64) -> Result<Quiz, String> {
    let v = get_all_quiz()?;
    let quiz_list = v
        .get("stat_status_pairs")
        .ok_or("Cannot found stat_status_pairs".to_string())?
        .as_array()
        .ok_or("Cannot as_array".to_string())?;

    let quiz_name = quiz_list
        .iter()
        .find_map(|v| {
            v.get("stat").map_or(None, |stat| {
                // fronted id
                stat.get("frontend_question_id").map_or(None, |v| {
                    if v.as_u64().unwrap() == id {
                        stat.get("question__title_slug")
                    } else {
                        None
                    }
                })
            })
        })
        .ok_or("Cannot found this id".to_string())?
        .as_str()
        .ok_or("Cannot parse to string".to_string())?;

    Quiz::get_by_name(quiz_name)
}

pub(super) fn get_random_quiz(l: Option<Level>) -> Result<Quiz, String> {
    let token = get_csrftoken("./vault/csrftoken")?; //:= need change to some other path
    let cli = make_client(&token, LC_RANDOM_QUIZ_API).map_err(|e| e.to_string())?;

    loop {
        let req = request_builder(
            &cli,
            Method::GET,
            LC_RANDOM_QUIZ_API,
            LC_ALL_QUIZ_REFERER,
            &token,
            String::new(),
        )
        .map_err(|e| e.to_string())?;

        // 302 will call new location immediatly
        match cli.execute(req).map_err(|e| e.to_string()) {
            Ok(c) => {
                let jump_to_url = c.url().as_str(); // this url
                match l.as_ref() {
                    Some(level) => {
                        let q = get_quiz_by_url(jump_to_url)?;
                        if q.quiz_level() == level {
                            return Ok(q);
                        } else {
                            continue;
                        }
                    }
                    None => return get_quiz_by_url(jump_to_url), // get this quiz,
                }
            }
            Err(e) => return Err(e.to_string()),
        }
    }
}

/// read csrftoken from file
fn get_csrftoken(path: impl AsRef<Path>) -> Result<String, String> {
    // cache it
    match CSRFTOKEN.try_lock() {
        Ok(ref mut t) => {
            if t.is_some() {
                return Ok(t.as_ref().unwrap().to_string());
            } else {
                let mut f = File::open(path).map_err(|e| e.to_string())?;
                let mut buffer = Vec::new();
                f.read_to_end(&mut buffer).map_err(|e| e.to_string())?;

                let token = String::from_utf8(buffer).map_err(|e| e.to_string())?;
                **t = Some(token.clone());

                Ok(token)
            }
        }
        // this part is meanless when app run. but for test.
        // need to handle err
        Err(_) => {
            let mut f = File::open(path).map_err(|e| e.to_string())?;
            let mut buffer = Vec::new();
            f.read_to_end(&mut buffer).map_err(|e| e.to_string())?;

            let token = String::from_utf8(buffer).map_err(|e| e.to_string())?;

            Ok(token)
        }
    }
}

fn make_client(cookie: &str, url: impl IntoUrl) -> reqwest::Result<Client> {
    let jar = Jar::default();
    jar.add_cookie_str(&(String::from("csrftoken=") + cookie), &url.into_url()?);
    Client::builder()
        .cookie_store(true)
        .cookie_provider(Arc::new(jar))
        .build()
}

fn request_builder(
    client: &Client,
    method: Method,
    endpoint: impl IntoUrl + Clone,
    referer: impl IntoUrl + Clone,
    token: &str,
    body: String,
) -> reqwest::Result<Request> {
    client
        .request(method, endpoint)
        .header("X-CSRFToken", token)
        .header("referer", referer.as_str())
        .header("Content-Type", "application/json")
        .body(body)
        .build()
}

#[cfg(test)]
mod tests {
    use reqwest::blocking::Client;

    use super::*;

    #[test]
    fn test_request_builder() {
        let cli = Client::builder().build().unwrap();
        let token = get_csrftoken("./vault/csrftoken").unwrap();
        let req = request_builder(
            &cli,
            Method::POST,
            "https://aaa/bb/namehere/",
            "https://aaa/bb/namehere/",
            &token,
            "haha".into(),
        )
        .unwrap();

        assert_eq!(
            String::from_utf8(req.body().unwrap().as_bytes().unwrap().to_vec()).unwrap(),
            "haha".to_string()
        );
    }

    #[test]
    fn test_make_json_body() {
        dbg!(LC_GRAPHQL_BODY[0].to_string() + "aaa" + LC_GRAPHQL_BODY[1]);
    }
}
