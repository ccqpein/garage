use string_calculate::*;

fn main() -> Result<(), String> {
    let a = QStr::new("123")?;
    let b = QStr::new("456")?;
    dbg!(&a);
    dbg!(&b);
    println!("{}", a + b);

    println!(
        "{}",
        QStr::new("77483297748937498327489237489379487239847293487")?
            * QStr::new("324329489325839274932847392875301981748375984728")?
    );

    Ok(())
}
