use super::*;
use telegram_bot::UserId;

/// receive message and return back
struct ChatGPT {
    // user or group
}

impl ChatGPT {
    fn new() -> Self {
        ChatGPT {}
    }

    /*
    command like "/chatgpt aaa" or /chatgpt@botid yoyoyo
     */
    fn parse_text(&self, text: &str) -> Result<Option<String>, String> {
        if text.starts_with("/chatgpt") {
            return Ok(Some(text.split(' ').skip(1).collect::<Vec<_>>().join(" ")));
        }
        Ok(None)
    }

    async fn start_new_session() {}

    async fn end_the_session() {}
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_text() {
        let x = ChatGPT::new();
        assert_eq!(
            Ok(Some(String::from("hello world"))),
            x.parse_text("/chatgpt hello world")
        );
    }
}
