fn reassign() -> Vec<i32> {
    let mut a = vec![];
    for _ in 0..10000 {
        a = vec![];
        for i in 0..200000 {
            a.push(i);
        }
    }
    a
}

fn main() {
    dbg!(reassign());
    //dbg!(clear());
}
