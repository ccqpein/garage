use std::io::Result;
use std::time::Instant;
use std::{
    ffi::OsString,
    path::Path,
    thread::{self, Thread},
};

use futures::future::join_all;
use thread::JoinHandle;
use tokio::{join, main, net::TcpListener};

fn read_all_files_in_this_dir(p: &Path) -> Result<Vec<OsString>> {
    use std::fs::*;
    let mut result = vec![];
    for entry in read_dir(p)? {
        let dir = entry?;
        let path = dir.path();
        if path.is_dir() {
            result.append(&mut read_all_files_in_this_dir(dir.path().as_path())?);
        } else {
            result.push(dir.path().into_os_string());
        }
    }
    Ok(result)
}

fn read_all_files_in_this_dir_2(p: &Path) -> Result<Vec<String>> {
    use std::fs::*;
    let mut result = vec![];
    for entry in read_dir(p)? {
        let dir = entry?;
        let path = dir.path();
        if path.is_dir() {
            result.append(&mut read_all_files_in_this_dir_2(dir.path().as_path())?);
        } else {
            result.push(dir.path().to_str().unwrap().to_owned());
        }
    }
    Ok(result)
}

fn read_all_lines_in_file(p: impl AsRef<Path>) -> usize {
    use std::fs::*;
    use std::io::prelude::*;
    use std::io::BufRead;

    let mut buf = vec![];
    let mut f: File = File::open(p).unwrap();
    f.read_to_end(&mut buf).unwrap();

    let mut count = 0;
    let mut ss = String::new();
    let mut buff = buf.as_slice();
    loop {
        match buff.read_line(&mut ss) {
            Ok(0) => break,
            Ok(_) => count += 1,
            Err(_) => (),
        }
        ss.clear()
    }
    count
}

fn read_all_lines_in_files(ps: Vec<String>) -> usize {
    ps.iter().map(|p| read_all_lines_in_file(p)).sum::<usize>()
}

// maybe cause too many files open
async fn read_all_lines_in_file_a(p: impl AsRef<Path>) -> usize {
    use tokio::fs::File;
    use tokio::io::*;

    let mut buf = vec![];
    let mut f: File = File::open(p).await.unwrap();
    f.read_to_end(&mut buf).await.unwrap();

    let mut count = 0;
    let mut ss = String::new();
    let mut buff = buf.as_slice();
    loop {
        match buff.read_line(&mut ss).await {
            Ok(0) => break,
            Ok(_) => count += 1,
            _ => panic!(),
        }
        ss.clear()
    }
    count
}

async fn read_all_lines_in_file_a_2(ps: Vec<String>) -> usize {
    use std::io::BufRead;
    use tokio::fs::File;
    use tokio::io::AsyncReadExt;

    let mut count = 0;
    for p in ps {
        let mut buf = vec![];
        let mut f: File = File::open(p).await.unwrap();
        f.read_to_end(&mut buf).await.unwrap();

        let mut ss = String::new();
        let mut buff = buf.as_slice();
        loop {
            match buff.read_line(&mut ss) {
                Ok(0) => break,
                Ok(_) => count += 1,
                Err(_) => (),
            }

            // buff.read_line(&mut ss)
            //     .await
            //     .expect("reading from cursor won't fail");
            // count += 1;
            ss.clear()
        }
    }
    count
}

#[tokio::main]
async fn main() -> Result<()> {
    let mut all_files: Vec<String> = read_all_files_in_this_dir_2(Path::new("."))?;

    let mut now = Instant::now();

    let a = all_files
        .iter()
        .map(|p| read_all_lines_in_file(p))
        .sum::<usize>();

    println!("Cost: {:?}, a: {}, sync", now.elapsed(), a);

    let b = {
        // 5 files a group
        let mut cache: Vec<Vec<String>> = vec![];
        let mut count = 0;
        let leng = all_files.len();
        while count < leng {
            if leng - count >= 5 {
                cache.push(all_files.drain(0..5).collect::<Vec<String>>());
            } else {
                cache.push(all_files.drain(0..leng - count).collect::<Vec<String>>());
            }
            count += 5
        }
        cache
    };

    now = Instant::now();

    let bb = b
        .iter()
        .map(|x| {
            let xx = x.clone();
            tokio::spawn(async move { read_all_lines_in_file_a_2(xx).await })
        })
        .collect::<Vec<_>>();

    let re = join_all(bb)
        .await
        .iter()
        .map(|h| h.as_ref().unwrap())
        .sum::<usize>();

    println!("Cost: {:?}, a: {}, async", now.elapsed(), re);

    now = Instant::now();

    let c = b
        .iter()
        .map(|x| {
            let xx = (*x).clone();
            thread::spawn(|| read_all_lines_in_files(xx))
        })
        .collect::<Vec<JoinHandle<usize>>>();

    let mut re1 = 0;
    for h in c {
        re1 += h.join().unwrap();
    }

    println!("Cost: {:?}, a: {}, mutli threads", now.elapsed(), re1);

    Ok(())
}
