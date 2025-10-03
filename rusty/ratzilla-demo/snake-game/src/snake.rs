//trait Direction {}

/// maybe reuse it in future some time. Make it a bit abstract
trait Snake<'s, Coord: 's> {
    /// get the body
    fn body(&'s self) -> impl Iterator<Item = &'s Coord>;

    /// this snake can grow
    /// grow the one pixel in the front of snake
    fn grow(&'s mut self);

    /// this snake move one step
    /// if the head eat the food, it should grow, or keep moving
    fn one_step(&'s mut self, food: &Coord);

    // /// return the head
    // fn head(&'s self) -> Coord;

    //fn direction(&'s self) -> impl Direction;
}

enum Dir {
    Up,
    Down,
    Left,
    Right,
}

struct SnakeWidget {
    body: Vec<(u32, u32)>,
    dir: Dir,
}

impl<'s> Snake<'s, (u32, u32)> for SnakeWidget {
    fn body(&'s self) -> impl Iterator<Item = &'s (u32, u32)> {
        self.body.iter()
    }

    fn grow(&'s mut self) {
        todo!()
    }

    fn one_step(&'s mut self, food: &(u32, u32)) {
        todo!()
    }

    // fn head(&'s self) -> (u32, u32) {
    //     todo!()
    // }

    //fn direction(&'s self) -> impl Direction {}
}
