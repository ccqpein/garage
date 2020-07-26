use std::fmt::Arguments;
use std::io::{repeat, stdout, Error, ErrorKind, Read, Result, Stdout, Write};
// enum Direction {
//     Right,
//     Left,
//     Up,
//     Down,
// }

// struct SnakeBody {
//     d: Direction,
// }

pub struct Frame {
    row: usize,
    col: usize,
    points: Stdout,
}

impl Frame {
    pub fn new(row: usize, col: usize) -> Self {
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

        Self {
            row,
            col,
            points: std_out,
        }
    }

    pub fn write(&mut self, row: usize, col: usize, s: String) -> Result<()> {
        if row >= self.row || col >= self.col {
            return Err(Error::new(ErrorKind::Other, "beyond boundary"));
        }
        //let format_str = self.move_to(row, col);
        if col == 0 {
            write!(
                self.points,
                "{}",
                format_args!("\x1b[s\x1b[{}A\r{}\x1b[u", self.row - row, s)
            )
        } else {
            write!(
                self.points,
                "{}",
                format_args!("\x1b[s\x1b[{}A\r\x1b[{}C{}\x1b[u", self.row - row, col, s)
            )
        };
        self.points.flush()
    }
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
    std_out.write(b"\x1b[s\x1b[9A\x1b[39D2\x1b[u").unwrap();
    //std_out.write(b"\x1b[9B\x1b[39C").unwrap();
    std_out.flush().unwrap();
}
