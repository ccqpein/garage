#![feature(const_evaluatable_checked)]

use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
struct Node<const D: usize> {
    coord: [isize; D],
    neighbours: Vec<Rc<RefCell<Self>>>,
}

impl<const D: usize> Node<D> {
    #[inline]
    fn new(coord: [isize; D]) -> Self {
        Self {
            coord,
            neighbours: vec![],
        }
    }

    fn add_neighbours(&mut self, neighbours: Vec<Rc<RefCell<Self>>>) {
        self.neighbours = neighbours
    }
}

struct MapConfig<const D: usize> {
    ranges: [(isize, isize); D],
    unit_size: isize,
}

impl<const D: usize> MapConfig<D> {
    fn new(ranges: [(isize, isize); D], unit_size: isize) -> Self {
        Self { ranges, unit_size }
    }
}

/// D is dimension of this map, 3 or 2.
struct Map<const D: usize> {
    /// key is coord
    table: HashMap<[isize; D], Rc<RefCell<Node<D>>>>,
}

impl<const D: usize> Map<D> {
    // fn new(start_point:[isize;D], end_point:[isize;D], unit_size:isize) {
    //     // dimension
    //     for dim in start_point.len(){

    //     }
    // }

    fn new(self) -> Map<D> {}
}

impl<const D: usize> From<&MapConfig<D>> for Map<D> {
    fn from(conf: &MapConfig<D>) -> Self {
        //let start
    }
}
