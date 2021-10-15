use lazy_static::*;
use regex::{Captures, Match, Regex};

const TITLE_REGEX_STR: &'static str = r"^(?P<HEAD>#+\s*)(?P<CONTENT>[^#]*)";

lazy_static! {
    static ref TITLE_REGEX: Regex = Regex::new(TITLE_REGEX_STR).unwrap();
}

fn capture_title(s: &str) -> Option<Captures> {
    TITLE_REGEX.captures(s)
}

/// match the # symbols in the front of title
fn pick_the_head<'t>(cap: &Captures<'t>) -> Result<Match<'t>, String> {
    cap.name("HEAD").ok_or("Cannot find HEAD group.".into())
}

/// count how many # in this title match
fn head_count(m: &Match) -> usize {
    m.as_str().chars().filter(|c| *c == '#').count()
}

fn pick_the_head_content<'t>(cap: &Captures<'t>) -> Result<String, String> {
    cap.name("CONTENT")
        .ok_or("Cannot find CONTENT group.".into())
        .map(|m| m.as_str().into())
}

#[cfg(test)]
mod tests {
    use super::*;

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
            //dbg!(pick_the_head(case));
            assert!(pick_the_head(&capture_title(case).unwrap()).is_ok())
        }
    }

    #[test]
    fn test_head_count() {
        let testcase = "#README";
        assert_eq!(
            head_count(&pick_the_head(&capture_title(testcase).unwrap()).unwrap()),
            1
        );

        let testcase = "## README";
        assert_eq!(
            head_count(&pick_the_head(&capture_title(testcase).unwrap()).unwrap()),
            2
        );

        let testcase = "### README #";
        assert_eq!(
            head_count(&pick_the_head(&capture_title(testcase).unwrap()).unwrap()),
            3
        );

        let testcase = "# README#";
        assert_eq!(
            head_count(&pick_the_head(&capture_title(testcase).unwrap()).unwrap()),
            1
        );

        let testcase = "# #   README #";
        assert_eq!(
            head_count(&pick_the_head(&capture_title(testcase).unwrap()).unwrap()),
            1
        );
    }

    #[test]
    fn test_pick_the_head_content() {
        let title = capture_title("#README").unwrap();
        assert_eq!(pick_the_head_content(&title), Ok("README".to_string()));

        let title = capture_title("## README").unwrap();
        assert_eq!(pick_the_head_content(&title), Ok("README".to_string()));

        let title = capture_title("# README").unwrap();
        assert_eq!(pick_the_head_content(&title), Ok("README".to_string()));

        // cannot accept # before readme
        let title = capture_title("# #  README #").unwrap();
        assert_eq!(pick_the_head_content(&title), Ok("".to_string()));

        // last space
        let title = capture_title("# README ##").unwrap();
        assert_eq!(pick_the_head_content(&title), Ok("README ".to_string()));
    }
}
