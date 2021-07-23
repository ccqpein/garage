use string_calculate::*;

#[test]
fn over_flow_test() -> Result<(), String> {
    assert_eq!(QStr::new("77483297748937498327489237489379487239847293487")?
               * QStr::new("324329489325839274932847392875301981748375984728")?, QStr::new("25130118390194850683412660070812499567781857251501568850850777425345322235463926730679245866536")?);

    assert_eq!(
        QStr::new("10000000000000000000000000000000000000000000000000000000")?
            + QStr::new("10000000000000000000000000000000000000000000000000000009")?,
        QStr::new("20000000000000000000000000000000000000000000000000000009")?
    );

    assert_eq!(
        QStr::new("10000000000000000000000000000000000000000000000000000000")?
            * QStr::new("10000000000000000000000000000000000000000000000000000009")?,
        QStr::new("100000000000000000000000000000000000000000000000000000090000000000000000000000000000000000000000000000000000000")?
    );

    assert_eq!(QStr::new("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")? * QStr::new("18")?, QStr::new("18000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")?);

    Ok(())
}

#[test]
fn normal_tests() -> Result<(), String> {
    assert_eq!(
        QStr::new("1000")? * QStr::new("5000")?,
        QStr::new("5000000")?
    );

    let a = QStr::new("123")?;
    let b = QStr::new("456")?;

    dbg!(&a);
    dbg!(&b);

    assert_eq!(QStr::new("579")?, a + b);

    Ok(())
}
