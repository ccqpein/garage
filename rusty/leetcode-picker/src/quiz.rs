use std::fmt;
use std::str::FromStr;

use super::content_parser::*;
use super::request::*;

use reqwest::blocking::Response;
use scraper::Html;

const LC_P: &str = "https://leetcode.com/problems/";

#[derive(Debug)]
pub struct Quiz {
    title: String,
    level: Level,
    content: serde_json::Value,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Level {
    Easy,
    Medium,
    Hard,
}

impl FromStr for Level {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Easy" | "easy" | "e" => Ok(Self::Easy),
            "Medium" | "medium" | "m" => Ok(Self::Medium),
            "Hard" | "hard" | "h" => Ok(Self::Hard),
            _ => Err("Unspport difficulty".to_string()),
        }
    }
}

impl Quiz {
    pub(super) fn from_resp(resp: Response) -> Result<Self, String> {
        let content = graphql_response_parse(resp)?;
        Ok(Quiz {
            title: find_question_title_from_graphql_req(&content)?,
            level: Level::from_str(&find_question_level_from_graphql_req(&content)?)?,
            content,
        })
    }

    pub fn get_by_name(name: &str) -> Result<Self, String> {
        get_quiz_by_url(&(LC_P.to_string() + name + "/"))
    }

    /// get quiz randomly
    pub fn get_randomly(level: Option<Level>) -> Result<Self, String> {
        get_random_quiz(level)
    }

    /// get quiz id
    pub fn get_by_id(id: u64) -> Result<Self, String> {
        get_quiz_by_id(id)
    }

    pub fn quiz_id(&self) -> Result<String, String> {
        find_question_id_from_graphql_req(&self.content)
    }

    pub fn quiz_pure_description(&self) -> Result<&str, String> {
        find_question_content(&self.content)
    }

    /// Get markdown description of quiz
    pub fn quiz_description(&self) -> Result<String, String> {
        let fragment = Html::parse_fragment(self.quiz_pure_description()?);
        Ok(description_markdown(description_in_graphql(&fragment)).join(""))
    }

    pub fn quiz_level(&self) -> &Level {
        &self.level
    }

    /// Get code snippet of special language
    /// if None, give first one
    pub fn code_snippet(&self, lang: &str) -> Option<&str> {
        match find_code_snippet(&self.content, lang) {
            Ok(d) => d,
            Err(e) => {
                println!("{}", e.to_string());
                None
            }
        }
    }
}

impl fmt::Display for Quiz {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}: {}\n\n{}",
            self.quiz_id().unwrap(),
            self.title,
            self.quiz_description().unwrap()
        )
    }
}
