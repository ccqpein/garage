use reqwest::blocking::Client;
use reqwest::blocking::Request;
use reqwest::blocking::{get, Response};
use reqwest::cookie::Jar;
use reqwest::Method;
use reqwest::{IntoUrl, Url};

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::sync::Arc;

const LC_GRAPHQL_ENDPOINT: &str = "https://leetcode.com/graphql";
const LC_GRAPHQL_BODY: [&str; 2] = [
    r#"{"operationName": "questionData", "variables": {"titleSlug": ""#,
    r#""},"query": "query questionData($titleSlug: String!) {question(titleSlug: $titleSlug) {questionId questionFrontendId boundTopicId title titleSlug content translatedTitle translatedContent isPaidOnly difficulty likes dislikes isLiked similarQuestions exampleTestcases contributors {username profileUrl avatarUrl __typename} topicTags {name slug translatedName __typename} companyTagStats codeSnippets {lang langSlug code __typename} stats hints solution {id canSeeDetail paidOnly hasVideoSolution paidOnlyVideo __typename} status sampleTestCase metaData judgerAvailable judgeType mysqlSchemas enableRunCode enableTestMode enableDebugger envInfo libraryUrl adminUrl __typename}}"}"#,
];

/// return response
pub fn get_quiz_by_url(url: &str) -> Result<Response, String> {
    let token = get_csrftoken("./vault/csrftoken")?; //:= need change to some other path
    let cli = make_client(&token, url).map_err(|e| e.to_string())?;
    let req = request_builder(&cli, Method::POST, url, &token).map_err(|e| e.to_string())?;
    cli.execute(req).map_err(|e| e.to_string())
}

/// read csrftoken from file
fn get_csrftoken(path: impl AsRef<Path>) -> Result<String, String> {
    let mut f = File::open(path).map_err(|e| e.to_string())?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer).map_err(|e| e.to_string())?;
    String::from_utf8(buffer).map_err(|e| e.to_string())
}

fn make_client(cookie: &str, url: impl IntoUrl) -> reqwest::Result<Client> {
    let jar = Jar::default();
    jar.add_cookie_str(cookie, &url.into_url()?);
    Client::builder()
        .cookie_store(true)
        .cookie_provider(Arc::new(jar))
        .build()
}

fn request_builder(
    client: &Client,
    method: Method,
    url: impl IntoUrl + Clone,
    token: &str,
) -> reqwest::Result<Request> {
    let name = url.as_str().rsplit('/').nth(1).unwrap();
    client
        .request(method, LC_GRAPHQL_ENDPOINT)
        .header("X-CSRFToken", token)
        .header("referer", url.as_str())
        .header("Content-Type", "application/json")
        .body(LC_GRAPHQL_BODY[0].to_string() + name + LC_GRAPHQL_BODY[1])
        .build()
}

#[cfg(test)]
mod tests {
    use reqwest::blocking::Client;

    use super::*;

    #[test]
    fn test_get_quiz_by_url() {
        dbg!(get_quiz_by_url(
            "https://leetcode.com/problems/capacity-to-ship-packages-within-d-days/"
        )
        .unwrap()
        .text()
        .unwrap());
    }

    #[test]
    fn test_request_builder() {
        let cli = Client::builder().build().unwrap();
        let token = get_csrftoken("./vault/csrftoken").unwrap();
        let req = request_builder(&cli, Method::POST, "https://aaa/bb/namehere/", &token).unwrap();

        dbg!(req.body().unwrap());
    }

    #[test]
    fn test_make_json_body() {
        dbg!(LC_GRAPHQL_BODY[0].to_string() + "aaa" + LC_GRAPHQL_BODY[1]);
    }
}
