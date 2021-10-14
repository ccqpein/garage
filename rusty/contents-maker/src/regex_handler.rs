use lazy_static::*;
use regex::Regex;

//const TITLE_REGEX_STR: &'static str = r"^(?P<HEAD>#+\s.*)";
const TITLE_REGEX_STR: &'static str = r"^(?P<HEAD>#+\s*)\w+";

lazy_static! {
    static ref TITLE_REGEX: Regex = Regex::new(TITLE_REGEX_STR).unwrap();
}

fn pick_the_head(content: &str) -> Option<regex::Match> {
    // match TITLE_REGEX.captures(content) {
    //     Some(cap) => cap.name("HEAD"),
    //     None => None,
    // }

    TITLE_REGEX.captures(content).unwrap().name("HEAD")
}

#[cfg(test)]
mod tests {
    use crate::regex_handler::pick_the_head;

    #[test]
    fn test_match_the_title() {
        let testcasees = vec![
            "#README",
            "# README",
            "# README#",
            "# README #",
            "#    README #",
        ];

        for case in testcasees {
            dbg!(pick_the_head(case));
        }
    }
}
