struct Cell<T: PartialEq> {
    inner: T,
}

enum Crowd<T: PartialEq> {
    Cell(Option<Cell<T>>),
    Grid(Box<Grid<T>>),
}

impl<T: PartialEq> Crowd<T> {
    fn is_cell(&self) -> bool {
        match self {
            Self::Cell(_) => true,
            _ => false,
        }
    }

    fn is_grid(&self) -> bool {
        match self {
            Self::Grid(_) => true,
            _ => false,
        }
    }
}

struct Potential<T: PartialEq> {
    /// store value and if it has picked before
    inner: Vec<(T, bool)>,
}

struct Grid<T: PartialEq> {
    data: [[Crowd<T>; 3]; 3], // 3 * 3
    potential_records: [[Potential<T>; 3]; 3],
}

impl<T: PartialEq> Grid<T> {
    /// only if crowd insided is Cell
    fn mutex_all(&self) -> Result<(), String> {
        if !self.data[0][0].is_cell() {
            return Err("grid should be cells".to_string());
        }
        //:= TODO
        todo!()
    }
}

fn solve<T>(g: &Grid<T>)
where
    T: PartialEq,
{
}
