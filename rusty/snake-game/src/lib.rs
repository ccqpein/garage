use std::io::{repeat, stdout, Read, Stdout, Write};

pub fn init_stdout(row: usize, col: usize) -> Stdout {
    let mut std_out = stdout();
    let mut a = vec![0; col];
    repeat(b'.').read_exact(&mut a).unwrap();
    a.push(b'\n');
    for _ in 0..row {
        std_out.write_all(&a).unwrap();
    }
    std_out
}
