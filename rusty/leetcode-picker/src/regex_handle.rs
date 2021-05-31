use lazy_static::*;
use regex::Regex;

/// pick the <code></code> in text
fn code_tag(text: &str) -> String {
    lazy_static! {
        static ref CODE_TAG: Regex = Regex::new(r"<code>(?P<data>.*?)</code>").unwrap();
    }

    String::from(CODE_TAG.replace_all(text, "`$data`"))
}

/// <sup></sup>
fn sup_tag(text: &str) -> String {
    lazy_static! {
        static ref SUP_TAG: Regex = Regex::new(r"<sup>(?P<data>.*?)</sup>").unwrap();
    }

    String::from(SUP_TAG.replace_all(text, "$data"))
}

/// <pre></pre>
fn pre_tag(text: &str) -> String {
    lazy_static! {
        static ref PRE_TAG: Regex = Regex::new(r"<pre>(?P<data>(.|\n|\t)*?)</pre>").unwrap();
    }

    String::from(PRE_TAG.replace_all(text, "```\n$data\n```"))
}

/// <ul></ul>
fn ul_tag(text: &str) -> String {
    lazy_static! {
        static ref UL_TAG: Regex = Regex::new(r"<ul>(?P<data>.*?)</ul>").unwrap();
    }

    String::from(UL_TAG.replace_all(text, "$data"))
}

/// <li></li>
fn li_tag(text: &str) -> String {
    lazy_static! {
        static ref LI_TAG: Regex = Regex::new(r"<li>(?P<data>.*?)</li>").unwrap();
    }

    String::from(LI_TAG.replace_all(text, "+ $data"))
}

/// pick the <strong></strong> in text
fn strong_tag(text: &str) -> String {
    lazy_static! {
        static ref STRONG_TAG: Regex = Regex::new(r"<strong>(?P<data>.*?)</strong>").unwrap();
    }

    String::from(STRONG_TAG.replace_all(text, "**$data**"))
}

fn special_sym_clean(text: &str) -> String {
    lazy_static! {
        static ref SPACE_TAG: Regex = Regex::new(r"(?P<data>\&nbsp;?)").unwrap();
        static ref LESS_TAG: Regex = Regex::new(r"(?P<data>\&lt;?)").unwrap();
        static ref NEWL_TAG: Regex = Regex::new(r"(?P<data>(\\+n)+?)").unwrap();
        static ref TAB_TAG: Regex = Regex::new(r"(?P<data>(\\+t)+?)").unwrap();
    }

    String::from(
        TAB_TAG.replace_all(
            NEWL_TAG
                .replace_all(
                    LESS_TAG
                        .replace_all(SPACE_TAG.replace_all(text, "").as_ref(), "<")
                        .as_ref(),
                    "\n",
                )
                .as_ref(),
            "  ",
        ),
    )
}

pub fn clean_all_tags(content: &mut String) {
    for f in [
        pre_tag,
        code_tag,
        sup_tag,
        ul_tag,
        li_tag,
        strong_tag,
        special_sym_clean,
    ] {
        *content = f(content);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_tag() {
        assert_eq!(
            code_tag("aaaaa ggg bb<code>sss</code>dd,ddd"),
            "aaaaa ggg bb`sss`dd,ddd"
        );
        assert_eq!(
            code_tag("aaaaa ggg bb<code>sss</code>dd,<code>ddd</code>"),
            "aaaaa ggg bb`sss`dd,`ddd`"
        );
    }

    #[test]
    fn test_strong_tag() {
        assert_eq!(
            strong_tag("aaaaa ggg bb<strong>sss</strong>dd,ddd"),
            "aaaaa ggg bb**sss**dd,ddd"
        );
        assert_eq!(
            strong_tag("aaaaa ggg bb<strong>sss</strong>dd,<strong>ddd</strong>"),
            "aaaaa ggg bb**sss**dd,**ddd**"
        );
    }

    #[test]
    fn test_li_tag() {
        dbg!(li_tag("+ a\n+ b"));
    }

    #[test]
    fn test_special_sym_clean() {
        assert_eq!(special_sym_clean("&nbsp;"), "");
        assert_eq!(special_sym_clean("&nbsp;&lt;="), "<=");
        assert_eq!(special_sym_clean("\n\n"), "\n\n");
        assert_eq!(special_sym_clean("\\n\\n"), "\n\n");
    }

    #[test]
    fn test_pre_tag() {
        assert_eq!(pre_tag("<pre>lalal</pre>"), "```\nlalal\n```");
        assert_eq!(pre_tag("<pre>**lala**</pre>"), "```\n**lala**\n```");
        assert_eq!(pre_tag("<pre>**lala**\n</pre>"), "```\n**lala**\n\n```");
    }
}
