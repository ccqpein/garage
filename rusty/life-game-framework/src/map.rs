use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
struct Node<T: Clone> {
    val: T,
}

impl<T: Clone> Node<T> {
    fn new(val: T) -> Self {
        Self { val }
    }
}

#[derive(Debug, Clone)]
enum Layer<T: Clone> {
    Space(Space<T>),
    Node(Node<T>),
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
struct Space<T>
where
    T: Clone,
{
    dimension: usize,

    /// from 0 to range
    range: usize,
    /// each size of unit
    unit_size: usize,

    layer: HashMap<usize, Layer<T>>,
}

impl<T: Clone> Space<T> {
    /// new empty map
    fn new(dim: usize, range: usize, unit_size: usize, init_val: T) -> Self {
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
    fn get(&self, index: impl AsRef<[usize]>) -> Option<&Layer<T>> {
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

    fn get_layer(&self, ind: &usize) -> Option<&Layer<T>> {
        if self.dimension == 0 {
            None
        } else {
            self.layer.get(ind)
        }
    }

    fn get_mut(&mut self, index: impl AsRef<[usize]>) -> Option<&mut Layer<T>> {
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

    fn get_layer_mut(&mut self, ind: &usize) -> Option<&mut Layer<T>> {
        if self.dimension == 0 {
            None
        } else {
            self.layer.get_mut(ind)
        }
    }

    /// count of nodes of this space
    fn count(&self) -> usize {
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
}

struct SpaceIter<'a, T: Clone> {
    index: [usize],
    max_index: [usize],
    inner: &'a Space<T>,
}

impl<'a, T> Iterator for &'a Space<T>
where
    T: Clone,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

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

        match m.get([1, 1, 1]) {
            Some(Layer::Node(n)) => assert_eq!(n.val, 100),
            _ => panic!(),
        }

        match m.get([1, 1, 0]) {
            Some(Layer::Node(n)) => assert_eq!(n.val, 10),
            _ => panic!(),
        }
    }
}
