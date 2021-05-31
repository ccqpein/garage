use leetcode_picker::*;

#[test]
fn work_flow_test() {
    let resp =
        get_quiz_by_url("https://leetcode.com/problems/capacity-to-ship-packages-within-d-days/")
            .unwrap();

    assert_eq!(resp.quiz_id().unwrap(), "1056");
    println!("{}", resp.quiz_description().unwrap()); // pretty print
}
