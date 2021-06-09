use std::fmt;
use std::str::FromStr;

use super::content_parser::*;
use super::request::*;

use reqwest::blocking::Response;
use scraper::Html;
use serde::Serialize;
use tinytemplate::TinyTemplate;

const LC_P: &str = "https://leetcode.com/problems/";

#[derive(Debug)]
pub struct Quiz {
    title: String,
    level: Level,
    source_link: String,
    content: serde_json::Value,
}

#[derive(Debug, Eq, PartialEq, Serialize, Clone)]
pub enum Level {
    Easy,
    Medium,
    Hard,
}

#[derive(Serialize)]
struct FmtTemplate {
    level: Level,
    source: String,
    title: String,
    content: String,
    code_snippet: String,
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
    pub(super) fn from_resp(resp: Response, source_link: String) -> Result<Self, String> {
        let content = graphql_response_parse(resp)?;
        Ok(Quiz {
            title: find_question_title_from_graphql_req(&content)?,
            level: Level::from_str(&find_question_level_from_graphql_req(&content)?)?,
            source_link,
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

    pub fn quiz_source(&self) -> &str {
        &self.source_link
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

    /// Parse content in template
    pub fn use_fmt_temp(&self, temp: &Option<String>) -> Result<String, String> {
        match temp {
            Some(s) => {
                // make template
                let mut tt = TinyTemplate::new();
                tt.add_template("quiz", s).map_err(|e| e.to_string())?;

                tt.render(
                    "quiz",
                    &FmtTemplate {
                        level: self.level.clone(),
                        source: self.source_link.clone(),
                        title: self.title.clone(),
                        content: self.quiz_description()?,
                        code_snippet: String::new(), //:= need input language
                    },
                )
                .map_err(|e| e.to_string())
            }
            None => Ok(format!("{}", self)), //default format
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
            self.quiz_description().unwrap(),
        )
    }
}

#[cfg(test)]
impl Quiz {
    fn new() -> Self {
        Self {
            title: String::new(),
            level: Level::Easy,
            source_link: String::new(),
            content: serde_json::Value::Null,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fmt_temp() {
        //let mut q = Quiz::new();
        let s = "README {source}, {title}; {content}..{level}";

        // make template
        let mut tt = TinyTemplate::new();
        tt.add_template("quiz", s)
            .map_err(|e| e.to_string())
            .unwrap();

        let result = tt
            .render(
                "quiz",
                &FmtTemplate {
                    level: Level::Easy,
                    source: "linklink".to_string(),
                    title: "titititle".to_string(),
                    content: "main content".to_string(),
                    code_snippet: String::new(),
                },
            )
            .unwrap();

        assert_eq!(result, "README linklink, titititle; main content..Easy");
    }
}
