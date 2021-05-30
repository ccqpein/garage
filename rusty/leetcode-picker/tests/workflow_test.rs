use leetcode_picker::*;

#[test]
fn work_flow_test() -> Result<(), String> {
    let rep =
        get_quiz_by_url("https://leetcode.com/problems/capacity-to-ship-packages-within-d-days/")?;

    response_parse(rep)?;
    Ok(())
}
