/// maybe reuse it in future some time. Make it a bit abstract
trait Snake<'s, Coord: 's> {
    fn body(&'s self) -> impl Iterator<Item = &'s Coord>;
    fn grow(&'s mut self);
    fn one_step(&'s mut self);
    fn head(&'s self) -> Coord;
}

struct SnakeWidget {
    body: Vec<(u32, u32)>,
}

impl<'s> Snake<'s, (u32, u32)> for SnakeWidget {
    fn body(&'s self) -> impl Iterator<Item = &'s (u32, u32)> {
        self.body.iter()
    }

    fn grow(&'s mut self) {
        todo!()
    }

    fn one_step(&'s mut self) {
        todo!()
    }

    fn head(&'s self) -> (u32, u32) {
        todo!()
    }
}
