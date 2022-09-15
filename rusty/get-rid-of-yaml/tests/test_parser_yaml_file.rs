use get_rid_of_yaml::parse0::*;

#[test]
fn test_parse_file() -> std::io::Result<()> {
    assert_eq!(
        parse_yaml_file("testcases/testcase0.yaml")?,
        V::new_l(vec![
            V::new_o(YAMLObject::new(
                "doe",
                V::new_singlev(r#""a deer, a female deer""#),
            )),
            V::new_o(YAMLObject::new(
                "ray",
                V::new_singlev(r#""a drop of golden sun""#),
            )),
            V::new_o(YAMLObject::new("pi", V::new_singlev(r#"3.14159"#))),
            V::new_o(YAMLObject::new("xmas", V::new_singlev(r#"true"#))),
            V::new_o(YAMLObject::new("french-hens", V::new_singlev(r#"3"#))),
            V::new_o(YAMLObject::new(
                "calling-birds",
                V::new_l(vec![
                    V::new_item("huey"),
                    V::new_item("dewey"),
                    V::new_item("louie"),
                    V::new_item("fred"),
                ]),
            )),
            V::new_o(YAMLObject::new(
                "xmas-fifth-day",
                V::new_l(vec![
                    V::new_o(YAMLObject::new("calling-birds", V::new_singlev("four"))),
                    V::new_o(YAMLObject::new("french-hens", V::new_singlev("3"))),
                    V::new_o(YAMLObject::new("golden-rings", V::new_singlev("5"))),
                    V::new_o(YAMLObject::new(
                        "partridges",
                        V::new_l(vec![
                            V::new_o(YAMLObject::new("count", V::new_singlev("1"))),
                            V::new_o(YAMLObject::new(
                                "location",
                                V::new_singlev(r#""a pear tree""#),
                            )),
                        ]),
                    )),
                    V::new_o(YAMLObject::new("turtle-doves", V::new_singlev("two"))),
                ]),
            )),
        ])
    );

    Ok(())
}
