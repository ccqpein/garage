#[derive(Debug)]
struct Node<const D: usize> {
    coord: [isize; D],
}

impl<const D: usize> Node<D> {
    #[inline]
    fn new(coord: [isize; D]) -> Self {
        Self { coord }
    }
}

/// D is dimension of this map, 3 or 2.
struct Map<const D: usize> {}
