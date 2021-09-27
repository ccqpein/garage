use std::collections::HashSet;
use std::hash::Hash;

#[derive(Clone)]
struct Cell<T: PartialEq + Clone> {
    inner: T,
}

impl<T: PartialEq + Clone> Cell<T> {
    fn new(inner: T) -> Self {
        Self { inner }
    }

    fn inner(&self) -> T {
        self.inner.clone()
    }
}

enum Crowd<T: PartialEq + Clone> {
    Cell(Option<Cell<T>>),
    Grid(Box<Grid<T>>),
}

impl<T: PartialEq + Clone> Crowd<T> {
    fn new() -> Self {}

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

    fn as_cell(&self) -> Option<&Cell<T>> {
        if let Self::Cell(c) = self {
            c.as_ref()
        } else {
            None
        }
    }
}

struct Potential<T: PartialEq + Clone> {
    /// store value and if it has picked before
    inner: Vec<(T, bool)>,
}

struct Grid<T: PartialEq + Clone> {
    data: [[Crowd<T>; 3]; 3], // 3 * 3

    /// only meaningful when crowd is cell
    potential_records: [[Potential<T>; 3]; 3],
}

impl<T: PartialEq + Clone + Hash + Eq> Grid<T> {
    /// only if crowd insided is Cell
    fn mutex_all(&self) -> Result<(), String> {
        if !self.data[0][0].is_cell() {
            return Err("grid should be cells".to_string());
        }
        //:= TODO
        let all_left = self
            .data
            .iter()
            .map(|row| row.iter().filter_map(|c| c.as_cell()).map(|c| c.inner()))
            .flatten()
            .collect::<HashSet<T>>();
        todo!()
    }
}

fn solve<T>(g: &Grid<T>)
where
    T: PartialEq + Clone,
{
}
