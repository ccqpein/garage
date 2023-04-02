use cl_format::*;

#[test]
fn play_groud_with_macro_0() {
    let a = cl_format!("~a, ~a, ~a", &1_i32, &2, &3);
    assert_eq!(String::from("1, 2, 3"), a.unwrap());

    let s = String::from("abc");
    let a = cl_format!("~a, ~a, ~a, ~S", &1_i32, &2, &3, &s);
    assert_eq!(String::from("1, 2, 3, \"abc\""), a.unwrap());

    let a = cl_format!("start ~a, ~a, ~a, ~a, here", &1_i32, &2, &3, &s);
    assert_eq!(String::from("start 1, 2, 3, abc, here"), a.unwrap());

    let ll: Vec<&dyn TildeAble> = vec![&1, &2, &3];
    let ll = Args::from(&ll);

    let a = cl_format!("~a, ~a, ~a, ~{~a,~}", &1_i32, &2, &3, &ll);
    assert_eq!(String::from("1, 2, 3, 1,2,3,"), a.unwrap());

    // reset the args
    ll.reset();
    let a = cl_format!("~a, ~a, ~a, ~{~a~^,~}", &1_i32, &2, &3, &ll);
    assert_eq!(String::from("1, 2, 3, 1,2,3"), a.unwrap());
}

#[test]
fn play_groud_with_macro_1() {
    let a = cl_format!("The value is: ~a", &1_i32);
    assert_eq!(String::from("The value is: 1"), a.unwrap());

    let s = String::from("foo");
    let a = cl_format!("The value is: ~a", &s);
    assert_eq!(String::from("The value is: foo"), a.unwrap());

    let l: Args = Args::from(&vec![&1 as &dyn TildeAble, &2, &3]);
    let a = cl_format!("The value is: ~a", &l);
    assert_eq!(String::from("The value is: 1"), a.unwrap());
}
