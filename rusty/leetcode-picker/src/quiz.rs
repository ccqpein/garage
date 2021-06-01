use super::request::*;
use super::to_markdown::*;
use reqwest::blocking::Response;
use scraper::Html;

const LC_P: &str = "https://leetcode.com/problems/";

pub struct Quiz {
    data: serde_json::Value,
}

pub enum Level {
    Easy,
    Medium,
    Hard,
}

impl Quiz {
    pub(super) fn from_resp(resp: Response) -> Result<Self, String> {
        Ok(Quiz {
            data: graphql_response_parse(resp)?,
        })
    }

    pub fn get_by_name(name: &str) -> Result<Self, String> {
        get_quiz_by_url(&(LC_P.to_string() + name + "/"))
    }

    /// get quiz randomly
    pub fn get_randomly(level: Option<Level>) -> Result<Self, String> {
        get_random_quiz()
    }

    pub fn quiz_id(&self) -> Result<String, String> {
        find_question_id_from_graphql_req(&self.data)
    }

    pub fn quiz_pure_description(&self) -> Result<&str, String> {
        find_question_content(&self.data)
    }

    /// Get markdown description of quiz
    pub fn quiz_description(&self) -> Result<String, String> {
        let fragment = Html::parse_fragment(self.quiz_pure_description()?);
        Ok(description_markdown(description_in_graphql(&fragment)).join(""))
    }
}
