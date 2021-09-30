use std::collections::HashSet;

struct Grid {
    data: [[Option<usize>; 3]; 3],
    potential_records: Option<[[Potential; 3]; 3]>,
}

impl Grid {
    fn new(inner: &[[Option<usize>; 3]; 3]) -> Self {
        let mut a = Self {
            data: *inner,
            potential_records: None,
        };
        a.init_potential();
        a
    }

    fn get_row(&self, row: usize) -> impl Iterator<Item = usize> + '_ {
        self.data[row].iter().filter_map(|c| *c)
    }

    fn get_col(&self, col: usize) -> impl Iterator<Item = usize> + '_ {
        self.data.iter().filter_map(move |r| r[col])
    }

    fn get_all(&self) -> impl Iterator<Item = usize> + '_ {
        //:= need check if has duplication
        (0..3).map(|r| self.get_row(r)).flatten()
    }

    fn init_potential(&mut self) {
        let all = self.get_all().collect::<HashSet<_>>();
        let p = (1..=9) // 1 to 9
            .into_iter()
            .collect::<HashSet<_>>()
            .difference(&all)
            .cloned()
            .collect::<Vec<_>>();
        for r in 0..3 {
            for c in 0..3 {
                if self.data[r][c].is_none() {
                    self.potential_records.as_mut().unwrap()[r][c] = Potential::new(&p);
                } else {
                    self.potential_records.as_mut().unwrap()[r][c] = Potential::new(&vec![]);
                }
            }
        }
    }
}

struct Potential {
    inner: Vec<(usize, bool)>,
}

impl Potential {
    fn placeholder() -> Self {
        Self { inner: vec![] }
    }

    fn new(inner: &Vec<usize>) -> Self {
        Self {
            inner: inner.iter().map(|x| (*x, false)).collect(),
        }
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
                    Potential::new(&vec![]),
                    Potential::new(&vec![]),
                    Potential::new(&vec![]),
                ],
                [
                    Potential::new(&vec![]),
                    Potential::new(&vec![]),
                    Potential::new(&vec![]),
                ],
                [
                    Potential::new(&vec![]),
                    Potential::new(&vec![]),
                    Potential::new(&vec![]),
                ],
            ],
        };
        //:= TODO
        dbg!(a.get_row(2).collect::<Vec<_>>());
        dbg!(a.get_col(2).collect::<Vec<_>>());
    }
}
