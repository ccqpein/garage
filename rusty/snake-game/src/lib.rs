use std::io::{repeat, stdout, Read, Stdout, Write};

enum Direction {
    Right,
    Left,
    Up,
    Down,
}

struct SnakeBody {
    d: Direction,
}

struct Frame {
    row: usize,
    col: usize,
    points: Vec<Vec<Option<SnakeBody>>>,
}

pub fn init_stdout(row: usize, col: usize) -> Stdout {
    let mut std_out = stdout();
    let mut a = vec![0; col];
    repeat(b'.').read_exact(&mut a).unwrap();
    a.push(b'\n');
    for i in 0..row {
        if i == row / 2 {
            // baby snake
            std_out.write_all(&a).unwrap();
            print!("\x1b[1A\x1b[{}C◼◼\n", col / 2 - 1);
        } else {
            std_out.write_all(&a).unwrap();
        }
        std_out.flush().unwrap();
    }
    std_out
}

pub fn back_to_origin(std_out: &mut Stdout) {
    std_out.write(b"\x1b[9A\x1b[39D2").unwrap();
    std_out.flush().unwrap();
}
