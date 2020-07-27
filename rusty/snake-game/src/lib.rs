use std::io::{repeat, stdout, Error, ErrorKind, Read, Result, Stdout, Write};

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
        for _ in 0..row {
            std_out.write_all(&a).unwrap();
            std_out.flush().unwrap();
        }

        Self {
            row,
            col,
            points: std_out,
        }
    }

    pub fn write(&mut self, row: i32, col: i32, s: String) -> Result<()> {
        if row < 0 || col < 0 {
            return Err(Error::new(ErrorKind::Other, "beyond boundary"));
        }

        if row as usize >= self.row || col as usize >= self.col {
            return Err(Error::new(ErrorKind::Other, "beyond boundary"));
        }

        if col == 0 {
            write!(
                self.points,
                "{}",
                format_args!("\x1b[s\x1b[{}A\r{}\x1b[u", self.row - row as usize, s)
            )
            .unwrap()
        } else {
            write!(
                self.points,
                "{}",
                format_args!(
                    "\x1b[s\x1b[{}A\r\x1b[{}C{}\x1b[u",
                    self.row - row as usize,
                    col,
                    s
                )
            )
            .unwrap()
        };
        self.points.flush()
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug)]
pub struct Snake {
    body: Vec<(i32, i32)>,
    dir: Direction,
}

impl Snake {
    pub fn new(head: (i32, i32), dir: Direction, length: usize) -> Self {
        let a = match dir {
            Direction::Up => (-1, 0),
            Direction::Down => (1, 0),
            Direction::Left => (0, 1),
            Direction::Right => (0, -1),
        };

        // make tail
        let b = (0..length)
            .map(|x| (head.0 + x as i32 * a.0, head.1 + x as i32 * a.1))
            .collect::<Vec<_>>();

        if !b.iter().all(|x| x.0 >= 0 && x.1 >= 0) {
            return Self { body: vec![], dir };
        }

        Self {
            body: b.iter().map(|x| (x.0, x.1)).collect::<Vec<_>>(),
            dir,
        }
    }

    /// can only show when the change happen, have bug of showing snake when the length > 2
    pub fn show(&mut self, frame: &mut Frame) -> Result<()> {
        for b in &self.body {
            frame.write(b.0, b.1, String::from("◼"))?;
        }
        Ok(())
    }

    pub fn move_one_step(&mut self) -> Result<Option<(i32, i32)>> {
        let step = match self.dir {
            Direction::Up => (1, 0),
            Direction::Down => (-1, 0),
            Direction::Left => (0, -1),
            Direction::Right => (0, 1),
        };

        let tail = self.body.pop().unwrap();
        let a = self.body.first().unwrap();
        let new_head = (a.0 as i32 + step.0, a.1 as i32 + step.1);

        self.body.insert(0, new_head);

        Ok(Some(tail))
    }
}
