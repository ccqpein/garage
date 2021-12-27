struct A<T> {
    inner: Vec<T>,
}

impl<T> A<T> {
    fn get_mut(&mut self, ind: usize) -> Option<&mut T> {
        self.inner.get_mut(ind)
    }

    fn test(&mut self) -> Vec<&mut T> {
        let mut result = vec![];
        for i in [0, 1, 2] {
            result.push(self.get_mut(i).unwrap());
        }
        result
    }
}
