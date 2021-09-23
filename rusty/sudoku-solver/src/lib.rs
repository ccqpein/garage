struct Cell<T: PartialEq> {
    inner: T,
}

enum Crowd<T: PartialEq> {
    Cell(Cell<T>),
    Grid(Box<Grid<T>>),
}

struct Potential<T: PartialEq> {
    /// store value and if it has picked before
    inner: Vec<(T, bool)>,
}

struct Grid<T: PartialEq> {
    data: [[Crowd<T>; 3]; 3],
    potential_records: [[Potential<T>; 3]; 3],
}

impl<T: PartialEq> Grid<T> {
    /// only if crowd inside is Cell
    fn mutex_all(&self) {
        //:= TODO
        todo!()
    }
}

fn solve<T>(g: &Grid<T>)
where
    T: PartialEq,
{
}
