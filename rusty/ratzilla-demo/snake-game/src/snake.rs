use std::collections::VecDeque;

enum Status {
    Eaten,  // eat one food
    Full,   // win
    Normal, // keep playing
    Lose,   // lose
}

// /// maybe reuse it in future some time. Make it a bit abstract
// trait Snake<'s, Coord: 's + Eq + PartialEq> {
//     /// get the body
//     fn body(&'s self) -> impl Iterator<Item = &'s Coord>;

//     // /// this snake can grow
//     // /// grow the one pixel in the front of snake
//     // fn grow(&'s mut self) -> Result<Status, String>;

//     /// this snake move one step
//     /// if the head eat the food, it should grow, or keep moving
//     fn one_step(&'s mut self, food: &Coord) -> Result<Status, String>;

//     /// the next coord the snake will reach
//     fn next_head(&'s self) -> Result<Option<Coord>, String>;

//     // fn push_front(&'s mut self, next: Coord) -> Result<Status, String>;

//     // fn pop_back(&'s mut self) -> Result<Status, String>;
// }

enum Dir {
    Up,
    Down,
    Left,
    Right,
}

struct SnakeWidget {
    body: VecDeque<(u32, u32)>,
    dir: Dir,

    /// row axis limit, 0 indexed
    row_limit: u32,
    /// col axis limit, 0 indexed
    col_limit: u32,
}

// impl<'s> Snake<'s, (u32, u32)> for SnakeWidget {
//     fn body(&'s self) -> impl Iterator<Item = &'s (u32, u32)> {
//         self.body.iter()
//     }

//     fn one_step(&'s mut self, food: &(u32, u32)) -> Result<Status, String> {
//         todo!()
//     }

//     fn next_head(&'s self) -> Result<Option<(u32, u32)>, String> {
//         let head = self.body.front().ok_or("snake is empty".to_string())?;
//         match self.dir {
//             Dir::Up => {
//                 if head.0 == 0 {
//                     Ok(None)
//                 } else {
//                     Ok(Some((head.0 - 1, head.1)))
//                 }
//             }
//             Dir::Down => {
//                 if head.0 == self.row_limit {
//                     Ok(None)
//                 } else {
//                     Ok(Some((head.0 + 1, head.1)))
//                 }
//             }
//             Dir::Left => {
//                 if head.1 == 0 {
//                     Ok(None)
//                 } else {
//                     Ok(Some((head.0, head.1 - 1)))
//                 }
//             }
//             Dir::Right => {
//                 if head.1 == self.col_limit {
//                     Ok(None)
//                 } else {
//                     Ok(Some((head.0, head.1 + 1)))
//                 }
//             }
//         }
//     }
// }

trait Snake {
    type Coord;
    /// get the body
    fn body<'s>(&'s self) -> impl Iterator<Item = &'s Self::Coord>;

    /// this snake move one step
    /// if the head eat the food, it should grow, or keep moving
    fn one_step(&mut self, food: &Self::Coord) -> Result<Status, String>;

    /// the next coord the snake will reach
    fn next_head(&self) -> Result<Option<Self::Coord>, String>;
}

impl Snake for SnakeWidget {
    type Coord = (u32, u32);

    fn body<'s>(&'s self) -> impl Iterator<Item = &'s Self::Coord> {
        self.body.iter()
    }

    fn one_step(&mut self, food: &Self::Coord) -> Result<Status, String> {
        let next_head = self.next_head()?;

        match next_head {
            Some(nh) => {
                if nh == *food {
                    self.body.push_front(nh);
                    return Ok(Status::Eaten);
                } else {
                    self.body.push_front(nh);
                    self.body.pop_back();
                    return Ok(Status::Normal);
                }
            }
            None => return Ok(Status::Lose),
        }
    }

    fn next_head(&self) -> Result<Option<Self::Coord>, String> {
        let head = self.body.front().ok_or("snake is empty".to_string())?;
        match self.dir {
            Dir::Up => {
                if head.0 == 0 {
                    Ok(None)
                } else {
                    Ok(Some((head.0 - 1, head.1)))
                }
            }
            Dir::Down => {
                if head.0 == self.row_limit {
                    Ok(None)
                } else {
                    Ok(Some((head.0 + 1, head.1)))
                }
            }
            Dir::Left => {
                if head.1 == 0 {
                    Ok(None)
                } else {
                    Ok(Some((head.0, head.1 - 1)))
                }
            }
            Dir::Right => {
                if head.1 == self.col_limit {
                    Ok(None)
                } else {
                    Ok(Some((head.0, head.1 + 1)))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::snake::{Dir, Snake, SnakeWidget};

    // #[test]
    // fn trait_impl_test() {
    //     // function
    //     fn a<'a>(_: impl Snake<'a, (u32, u32)>) {}
    //     a(SnakeWidget {
    //         body: VecDeque::new(),
    //         dir: Dir::Up,
    //         row_limit: 1,
    //         col_limit: 1,
    //     })
    // }
}
