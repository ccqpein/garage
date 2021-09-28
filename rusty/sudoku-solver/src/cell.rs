struct Grid {
    data: [[Option<usize>; 3]; 3],
    potential_records: [[Potential; 3]; 3],
}

impl Grid {
    fn get_row(&self, row: usize) -> impl Iterator<Item = usize> + '_ {
        self.data[row].iter().filter_map(|c| *c)
    }

    //fn get_col(&self) -> Iterator<Item = usize> {}
}

struct Potential {
    inner: Vec<(usize, bool)>,
}

impl Potential {
    fn new(inner: Vec<(usize, bool)>) -> Self {
        Self { inner }
    }
}

struct Map {
    grids: [[Grid; 3]; 3],
}

impl Map {
    fn get_row() {}
    fn get_col() {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grid_get_row() {
        let a = Grid {
            data: [
                [Some(1), Some(2), None],
                [Some(3), Some(2), None],
                [Some(4), Some(2), None],
            ],
            potential_records: [
                [
                    Potential::new(vec![]),
                    Potential::new(vec![]),
                    Potential::new(vec![]),
                ],
                [
                    Potential::new(vec![]),
                    Potential::new(vec![]),
                    Potential::new(vec![]),
                ],
                [
                    Potential::new(vec![]),
                    Potential::new(vec![]),
                    Potential::new(vec![]),
                ],
            ],
        };
        //:= TODO
        dbg!(a.get_row(2).collect::<Vec<_>>());
    }
}
