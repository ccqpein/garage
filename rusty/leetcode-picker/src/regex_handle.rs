use lazy_static::*;
use regex::Regex;

/// pick the <code></code> in text
fn code_tag(text: &str) -> Vec<&str> {
    lazy_static! {
        static ref CODE_TAG: Regex = Regex::new(r"</{0,1}code>").unwrap();
    }

    CODE_TAG.split(text).collect::<Vec<&str>>()
}

///:= TODO: <sup></sup>

/// pick the <strong></strong> in text
fn strong_tag(text: &str) -> Vec<&str> {
    lazy_static! {
        static ref STRONG_TAG: Regex = Regex::new(r"</{0,1}strong>").unwrap();
    }

    STRONG_TAG.split(text).collect::<Vec<&str>>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_tag() {
        dbg!(code_tag("aaaaa ggg bb<code>sss</code>dd,ddd"));
        dbg!(code_tag("aaaaa ggg bb<code>sss</code>dd,<code>ddd</code>"));
    }
}
