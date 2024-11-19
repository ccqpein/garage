#![feature(cow_is_borrowed)]
use std::{borrow::Cow, time::Instant};

fn test1() {
    let now = Instant::now();
    let a: Vec<Vec<Option<i32>>> = vec![vec![None; 50000]; 50000];
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 1.857321917s
    dbg!(a
        .into_iter()
        .map(|aa| aa.into_iter().map(|aaa| aaa.unwrap_or(0)).sum::<i32>())
        .sum::<i32>());
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 24.437415167s
}

// Cow from borrow
fn test2() {
    let now = Instant::now();
    let seed: Vec<Option<i32>> = vec![None; 50000];
    let c = Cow::from(&seed);
    dbg!(c.is_owned());
    let a: Vec<Cow<[Option<i32>]>> = vec![c; 50000];
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 390.583µs
    dbg!(a
        .into_iter()
        .map(|aa| aa.into_iter().map(|aaa| aaa.unwrap_or(0)).sum::<i32>())
        .sum::<i32>());
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 15.478458542s
}

// Cow from borrow with iter()
fn test2_2() {
    let now = Instant::now();
    let seed: Vec<Option<i32>> = vec![None; 50000];
    let c = Cow::from(&seed);
    dbg!(c.is_owned());
    let a: Vec<Cow<[Option<i32>]>> = vec![c; 50000];
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 463.083µs
    dbg!(a
        .iter()
        .map(|aa| aa.iter().map(|aaa| aaa.unwrap_or(0)).sum::<i32>())
        .sum::<i32>());
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 15.384387083s
}

// Cow from owned
fn test3() {
    let now = Instant::now();
    let seed: Vec<Option<i32>> = vec![None; 50000];
    let c = Cow::from(seed);
    dbg!(c.is_owned());
    let a: Vec<Cow<[Option<i32>]>> = vec![c; 50000];
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 1.761242292s
    dbg!(a
        .into_iter()
        .map(|aa| aa.into_iter().map(|aaa| aaa.unwrap_or(0)).sum::<i32>())
        .sum::<i32>());
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 19.014466209s
}

// just one more step of seed
fn test4() {
    let now = Instant::now();
    let seed: Vec<Option<i32>> = vec![None; 50000];
    let a: Vec<Vec<Option<i32>>> = vec![seed; 50000];
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 1.749564458s
    dbg!(a
        .into_iter()
        .map(|aa| aa.into_iter().map(|aaa| aaa.unwrap_or(0)).sum::<i32>())
        .sum::<i32>());
    println!("time cost: {:?}", Instant::now().duration_since(now)); // 24.785834709s
}

fn main() {
    //test1();
    //test2();
    //test2_2();
    //test3();
    test4();
}
