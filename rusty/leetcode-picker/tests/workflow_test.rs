use leetcode_picker::*;

#[test]
fn work_flow_test() {
    let resp = match Quiz::get_by_name("capacity-to-ship-packages-within-d-days") {
        Ok(v) => v,
        Err(e) => {
            println!("{:?}", e);
            panic!();
        }
    };

    assert_eq!(resp.quiz_id().unwrap(), "1011");
    //println!("{}", resp.quiz_description().unwrap()); // pretty print
}

#[test]
fn pick_by_id_work_flow_test() {
    let resp = Quiz::get_by_id(1011).unwrap();
    //dbg!(&resp);
    assert_eq!(resp.quiz_id().unwrap(), "1011");
}

#[test]
fn pick_by_level_work_flow_test() {
    let resp = Quiz::get_randomly(Some(Level::Easy)).unwrap();
    assert_eq!(resp.quiz_level(), &Level::Easy);
}
