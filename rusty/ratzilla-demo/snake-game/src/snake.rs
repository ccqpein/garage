use rand::{rngs::ThreadRng, seq::IndexedRandom};
use std::{
    cell::RefCell,
    collections::{HashSet, VecDeque},
    rc::Rc,
};

use web_time::{Duration, Instant};

#[derive(Debug, Eq, PartialEq)]
pub enum Status {
    Eaten,  // eat one food
    Normal, // keep playing
    Lose,   // lose
}

pub enum Dir {
    Up,
    Down,
    Left,
    Right,
}

impl Dir {
    pub fn is_up(&self) -> bool {
        match self {
            Dir::Up => true,
            _ => false,
        }
    }
    pub fn is_down(&self) -> bool {
        match self {
            Dir::Down => true,
            _ => false,
        }
    }
    pub fn is_left(&self) -> bool {
        match self {
            Dir::Left => true,
            _ => false,
        }
    }
    pub fn is_right(&self) -> bool {
        match self {
            Dir::Right => true,
            _ => false,
        }
    }
}

pub struct SnakeWidget {
    pub body: VecDeque<(u16, u16)>,
    pub dir: Rc<RefCell<Dir>>,

    // X -->
    // Y
    // |
    // v
    /// x axis limit, 1 indexed
    pub x_limit: u16,
    /// y axis limit, 1 indexed
    pub y_limit: u16,

    rng: ThreadRng,
    whole_board: HashSet<(u16, u16)>,

    // the easiest way to modify the speed
    duration: Duration,
    last_move_time: Instant,
}

impl SnakeWidget {
    pub fn new(x_limit: u16, y_limit: u16, dir: Rc<RefCell<Dir>>) -> Result<Self, String> {
        let a = x_limit / 2;
        let b = y_limit / 2;

        if b == y_limit - 1 {
            return Err("too small row limit".to_string());
        }

        let whole_board = (0..x_limit)
            .map(|x| (0..y_limit).map(move |y| (x, y)))
            .flatten()
            .collect();

        Ok(Self {
            body: vec![(a, b), (a + 1, b)].into(),
            dir: dir,
            x_limit,
            y_limit,
            rng: rand::rng(),
            whole_board,
            duration: Duration::from_millis(1000),
            last_move_time: Instant::now(),
        })
    }
}

pub(crate) trait Snake {
    type Coord;
    /// get the body
    fn body<'s>(&'s self) -> impl Iterator<Item = &'s Self::Coord>;

    /// this snake move one step
    /// if the head eat the food, it should grow, or keep moving
    fn one_step(&mut self, food: &Self::Coord) -> Result<Status, String>;

    /// the next coord the snake will reach
    fn next_head(&self) -> Result<Option<Self::Coord>, String>;

    /// new food, need snake to find the new food in case it random on the snake body
    fn new_food(&mut self) -> Option<Self::Coord>;
}

impl Snake for SnakeWidget {
    type Coord = (u16, u16);

    fn body<'s>(&'s self) -> impl Iterator<Item = &'s Self::Coord> {
        self.body.iter()
    }

    fn one_step(&mut self, food: &Self::Coord) -> Result<Status, String> {
        if Instant::now().duration_since(self.last_move_time) <= self.duration {
            return Ok(Status::Normal);
        }

        let next_head = self.next_head()?;

        match next_head {
            Some(nh) => {
                if nh == *food {
                    self.body.push_front(nh);
                    self.duration = self.duration - Duration::from_millis(50);
                    self.last_move_time = Instant::now();
                    return Ok(Status::Eaten);
                } else {
                    self.body.push_front(nh);
                    self.body.pop_back();
                    self.last_move_time = Instant::now();
                    return Ok(Status::Normal);
                }
            }
            None => return Ok(Status::Lose),
        }
    }

    fn next_head(&self) -> Result<Option<Self::Coord>, String> {
        // X --->
        // Y
        // |
        // v
        let head = self.body.front().ok_or("snake is empty".to_string())?;
        match *self.dir.borrow() {
            Dir::Up => {
                if head.1 == 0 {
                    Ok(None)
                } else {
                    Ok(Some((head.0, head.1 - 1)))
                }
            }
            Dir::Down => {
                if head.1 == self.y_limit - 1 {
                    Ok(None)
                } else {
                    Ok(Some((head.0, head.1 + 1)))
                }
            }
            Dir::Left => {
                if head.0 == 0 {
                    Ok(None)
                } else {
                    Ok(Some((head.0 - 1, head.1)))
                }
            }
            Dir::Right => {
                if head.0 == self.x_limit - 1 {
                    Ok(None)
                } else {
                    Ok(Some((head.0 + 1, head.1)))
                }
            }
        }
    }

    fn new_food(&mut self) -> Option<Self::Coord> {
        let all_left = self
            .whole_board
            .iter()
            .filter(|(x, y)| !self.body.contains(&(*x, *y)))
            .cloned()
            .collect::<Vec<_>>();

        all_left.choose(&mut self.rng).copied()
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::snake::{Dir, Snake, SnakeWidget, Status};

    #[test]
    fn next_head_test() {
        let mut sw = SnakeWidget::new(10, 10, Rc::new(RefCell::new(Dir::Up))).unwrap();

        let nh = sw.next_head();
        assert!(nh.is_ok());
        let nh = nh.unwrap();
        assert!(nh.is_some());
        assert_eq!(nh.unwrap(), (4u16, 5u16));

        assert!(sw.one_step(&(0, 0)).is_ok());
        assert_eq!(sw.body().collect::<Vec<_>>(), vec![&(4, 5), &(5, 5)]);

        assert!(sw.one_step(&(0, 0)).is_ok());
        assert_eq!(sw.body().collect::<Vec<_>>(), vec![&(3, 5), &(4, 5)]);

        // turn sw to left
        sw.dir = Rc::new(RefCell::new(Dir::Left));
        assert!(sw.one_step(&(0, 0)).is_ok());
        assert_eq!(sw.body().collect::<Vec<_>>(), vec![&(3, 4), &(3, 5)]);

        // eat the food
        assert_eq!(sw.one_step(&(3, 3)), Ok(Status::Eaten));
        assert_eq!(
            sw.body().collect::<Vec<_>>(),
            vec![&(3, 3), &(3, 4), &(3, 5)]
        );

        assert!(sw.one_step(&(0, 0)).is_ok());
        assert!(sw.one_step(&(0, 0)).is_ok());
        assert!(sw.one_step(&(0, 0)).is_ok());
        //dbg!(sw.body().collect::<Vec<_>>());
        assert_eq!(sw.one_step(&(0, 0)), Ok(Status::Lose));
    }
}
