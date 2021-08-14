use std::{cell::RefCell, collections::HashMap, path::Iter, rc::Rc};

#[derive(Debug, Clone)]
pub struct Node<T: Clone> {
    val: T,
}

impl<T: Clone> Node<T> {
    fn new(val: T) -> Self {
        Self { val }
    }
}

#[derive(Debug, Clone)]
pub enum Layer<T: Clone> {
    Space(Space<T>), // >0 dimension
    Node(Node<T>),   // 0 dimension
}

impl<T: Clone> Layer<T> {
    fn get(&self, index: impl AsRef<[usize]>) -> Option<&Layer<T>> {
        if index.as_ref().is_empty() {
            Some(&self)
        } else {
            match self {
                Layer::Space(m) => m.get(index),
                re @ Layer::Node(_) => Some(re),
            }
        }
    }

    fn get_mut(&mut self, index: impl AsRef<[usize]>) -> Option<&mut Layer<T>> {
        if index.as_ref().is_empty() {
            Some(self)
        } else {
            match self {
                Layer::Space(m) => m.get_mut(index),
                re @ Layer::Node(_) => Some(re),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Space<T>
where
    T: Clone,
{
    /// how many dimension
    dimension: usize,

    /// from 0 to range
    range: usize,
    /// each size of unit
    unit_size: usize,

    layer: HashMap<usize, Layer<T>>,
}

impl<T: Clone> Space<T> {
    /// new empty map
    pub fn new(dim: usize, range: usize, unit_size: usize, init_val: T) -> Self {
        if dim < 1 {
            panic!("dimension less than 1");
        }

        if dim == 1 {
            Space {
                dimension: dim,
                range,
                unit_size,
                layer: {
                    (0..range)
                        .into_iter()
                        .step_by(unit_size)
                        .map(|d| (d, Layer::Node(Node::new(init_val.clone()))))
                        .collect::<HashMap<usize, Layer<T>>>()
                },
            }
        } else {
            Space {
                dimension: dim,
                range,
                unit_size,
                layer: {
                    (0..range)
                        .into_iter()
                        .step_by(unit_size)
                        .map(|d| {
                            (
                                d,
                                Layer::Space(Space::new(
                                    dim - 1,
                                    range,
                                    unit_size,
                                    init_val.clone(),
                                )),
                            )
                        })
                        .collect::<HashMap<usize, Layer<T>>>()
                },
            }
        }
    }

    /// recursively get layer
    pub fn get(&self, index: impl AsRef<[usize]>) -> Option<&Layer<T>> {
        match index.as_ref().get(0) {
            Some(first) => match self.get_layer(first) {
                re @ Some(next) => match index.as_ref().get(1..) {
                    Some(tail) => next.get(tail),
                    None => re,
                },
                None => None,
            },
            None => None,
        }
    }

    pub fn get_layer(&self, ind: &usize) -> Option<&Layer<T>> {
        if self.dimension == 0 {
            None
        } else {
            self.layer.get(ind)
        }
    }

    pub fn get_mut<'a>(&'a mut self, index: impl AsRef<[usize]>) -> Option<&'a mut Layer<T>> {
        match index.as_ref().get(0) {
            Some(first) => match self.get_layer_mut(first) {
                Some(next) => match index.as_ref().get(1..) {
                    Some(tail) => next.get_mut(tail),
                    None => Some(next),
                },
                None => None,
            },
            None => None,
        }
    }

    pub fn get_layer_mut(&mut self, ind: &usize) -> Option<&mut Layer<T>> {
        if self.dimension == 0 {
            None
        } else {
            self.layer.get_mut(ind)
        }
    }

    /// count of nodes of this space
    pub fn count(&self) -> usize {
        if self.dimension == 1 {
            self.layer.keys().count()
        } else {
            self.layer
                .values()
                .map(|v| match v {
                    Layer::Space(m) => m.count(),
                    Layer::Node(_) => unreachable!(),
                })
                .sum()
        }
    }

    pub fn iter(&self) -> SpaceIter<'_, T> {
        SpaceIter {
            inner: self,

            // start
            index: vec![0; self.dimension],
            start: vec![0; self.dimension],
            done: false,

            max_index: vec![self.range; self.dimension],
            unit_size: self.unit_size,
        }
    }
}

fn index_helper<'a>(center: &'a [usize], offset: &usize) -> Vec<Vec<usize>> {
    if center.len() == 0 {
        return vec![vec![]];
    }

    let this: Vec<Vec<usize>> = if center[0] < *offset {
        vec![vec![center[0] + offset], vec![center[0]]]
    } else {
        vec![
            vec![center[0] + offset],
            vec![center[0]],
            vec![center[0] - offset],
        ]
    };

    index_helper(&center[1..], offset)
        .into_iter()
        .map(|n| {
            this.clone()
                .iter()
                .map(|inner| {
                    let mut ii = inner.clone();
                    ii.extend_from_slice(&n);
                    ii
                })
                .collect::<Vec<Vec<_>>>()
        })
        .flatten()
        .collect()
}

/* iter below */

//
pub struct SpaceIter<'a, T: Clone> {
    inner: &'a Space<T>,

    /// current index
    index: Vec<usize>,
    start: Vec<usize>,
    done: bool,

    /// the range of inner
    max_index: Vec<usize>,

    unit_size: usize,
}

impl<'a, T> Iterator for SpaceIter<'a, T>
where
    T: Clone,
{
    type Item = SpaceIterLoc<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done == true {
            return None;
        }

        let re = match self.inner.get(&self.index) {
            Some(Layer::Node(n)) => Some(SpaceIterLoc::new(
                self.index.clone(),
                &self.inner.unit_size,
                n,
                self.inner,
            )),
            _ => None,
        };

        for dim in (0..self.index.len()).rev() {
            self.index[dim] += self.unit_size;
            if self.index[dim] < self.max_index[dim] {
                break;
            } else {
                self.index[dim] = 0;
            }
        }

        if self.index == self.start {
            self.done = true;
        }

        re
    }
}

#[derive(Debug)]
pub struct SpaceIterLoc<'a, T: Clone> {
    current_index: Vec<usize>,
    step: &'a usize,
    inner_node: &'a Node<T>,
    inner_space: &'a Space<T>,
}

impl<'a, T: Clone> SpaceIterLoc<'a, T> {
    fn new(
        current_index: Vec<usize>,
        step: &'a usize,
        inner_node: &'a Node<T>,
        inner_space: &'a Space<T>,
    ) -> Self {
        Self {
            current_index,
            step,
            inner_node,
            inner_space,
        }
    }

    // get all neighbours of this node
    pub fn neighbour<'b: 'a>(&'b self) -> impl Iterator<Item = &'a Layer<T>> {
        index_helper(&self.current_index, self.step)
            .into_iter()
            .filter(move |n| **n != self.current_index)
            .filter_map(move |coord| self.inner_space.get(coord))
    }

    pub fn current_index(&self) -> &Vec<usize> {
        &self.current_index
    }
}

// iter_mut below

pub struct SpaceIterMut<'a, T: Clone> {
    inner: &'a mut Space<T>,

    /// current index
    index: Vec<usize>,
    start: Vec<usize>,
    done: bool,

    /// the range of inner
    max_index: Vec<usize>,

    unit_size: usize,
}

impl<'b, 'a: 'b, T: Clone> SpaceIterMut<'a, T> {
    fn get_mut(&'b mut self) -> Option<&'b mut Layer<T>> {
        self.inner.get_mut(&self.index)
    }
}

impl<'a, T> Iterator for SpaceIterMut<'a, T>
where
    T: Clone,
{
    type Item = &mut Layer<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done == true {
            return None;
        }

        let re = self.get_mut();
        for dim in (0..self.index.len()).rev() {
            self.index[dim] += self.unit_size;
            if self.index[dim] < self.max_index[dim] {
                break;
            } else {
                self.index[dim] = 0;
            }
        }

        if self.index == self.start {
            self.done = true;
        }

        re
    }
}

// #[derive(Debug)]
// pub struct SpaceIterLocMut<'a, T: Clone> {
//     current_index: Vec<usize>,
//     step: &'a usize,
//     inner_node: &'a mut Layer<T>,
//     inner_space: &'a mut Space<T>,
// }

// impl<'a, T: Clone> SpaceIterLocMut<'a, T> {
//     fn new(
//         current_index: Vec<usize>,
//         step: &'a usize,
//         inner_node: &'a mut Node<T>,
//         inner_space: &'a mut Space<T>,
//     ) -> Self {
//         Self {
//             current_index,
//             step,
//             inner_node,
//             inner_space,
//         }
//     }

//     // get all neighbours of this node
//     pub fn neighbour<'b: 'a>(&'b self) -> impl Iterator<Item = &'a Layer<T>> {
//         index_helper(&self.current_index, self.step)
//             .into_iter()
//             .filter(move |n| **n != self.current_index)
//             .filter_map(move |coord| self.inner_space.get(coord))
//     }

//     pub fn current_index(&self) -> &Vec<usize> {
//         &self.current_index
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_map_get() {
        let m = Space::new(3, 3, 1, 10);
        //dbg!(&m);
        //dbg!([0,].get(1..)); // Some([])
        //dbg!([0,].get(2..)); // None
        assert_eq!(m.count(), 27);
        assert!(m.get([]).is_none());

        match m.get([0, 1, 1]) {
            Some(&Layer::Node(Node { val })) => assert_eq!(val, 10),
            _ => panic!("shouldn't be none or map"),
        };
    }

    #[test]
    fn test_new_map_get_mut() {
        let mut m = Space::new(3, 3, 1, 10);
        match m.get_mut([1, 1, 1]) {
            Some(Layer::Node(n)) => n.val = 100,
            _ => panic!(),
        }

        match m.get_mut([1, 1]) {
            Some(Layer::Space(n)) => match n.get([1]) {
                Some(Layer::Node(m)) => assert_eq!(m.val, 100),
                _ => panic!(),
            },
            _ => panic!(),
        }

        match m.get([1, 1, 1]) {
            Some(Layer::Node(n)) => assert_eq!(n.val, 100),
            _ => panic!(),
        }

        match m.get([1, 1, 0]) {
            Some(Layer::Node(n)) => assert_eq!(n.val, 10),
            _ => panic!(),
        }

        match m.get([1, 1, 2]) {
            Some(Layer::Node(n)) => assert_eq!(n.val, 10),
            _ => panic!(),
        }
    }

    #[test]
    fn test_space_iter() {
        let m = Space::new(3, 3, 1, 10);
        let a = SpaceIter {
            inner: &m,
            index: vec![0, 0, 0],
            start: vec![0, 0, 0],
            done: false,
            max_index: vec![3, 3, 3],
            unit_size: 2, // not as same as Space size on purpose
        };

        let mut m_iter = m.iter();
        m_iter.next();
        assert_eq!(*m_iter.next().unwrap().current_index(), vec![0, 0, 1]);
        assert_eq!(*m_iter.next().unwrap().current_index(), vec![0, 0, 2]);
        assert_eq!(*m_iter.next().unwrap().current_index(), vec![0, 1, 0]);

        assert_eq!(*m.iter().last().unwrap().current_index(), vec![2, 2, 2]);
    }

    #[test]
    fn test_index_helper() {
        let testcase = [1, 1, 1];
        assert_eq!(index_helper(&testcase, &1).len(), 27);
    }
}
