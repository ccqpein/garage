mod pdk;

use extism_pdk::*;
use pdk::types::{CallToolResult, Content, ContentType, ToolDescription};
use pdk::*;
use serde_json::json;

#[derive(Debug)]
struct CustomError(String);

impl std::fmt::Display for CustomError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for CustomError {}

fn echo(input: String) -> String {
    input + "back"
}

pub(crate) fn call(input: types::CallToolRequest) -> Result<types::CallToolResult, Error> {
    let args = input.params.arguments.unwrap_or_default();
    let name = args.get("name").unwrap().as_str().unwrap();
    let input_content = args.get("input").unwrap().as_str().unwrap();
    match name {
        "echo" => Ok(CallToolResult {
            content: vec![Content {
                text: Some(echo(input_content.to_string())),
                r#type: ContentType::Text,
                ..Default::default()
            }],
            is_error: Some(false),
        }),
        _ => Err(Error::new(CustomError("unknown command".to_string()))),
    }
}

pub(crate) fn describe() -> Result<types::ListToolsResult, Error> {
    Ok(types::ListToolsResult {
        tools: vec![ToolDescription {
            name: "echo".into(),
            description: "Just Echo the input. It provides the following operations:
- `echo`: receive the input then echo it out.

"
            .into(),
            input_schema: json!({
                "type": "object",
                "required": ["name"],
                "properties": {
                    "name": {
                        "type": "string",
                        "description": "The name of the operation to perform. ",
                        "enum": ["echo"],
                    },
                    "input": {
                        "type": "string",
                        "description": "input of echo tool",
                    },

                },
            })
            .as_object()
            .unwrap()
            .clone(),
        }],
    })
}
