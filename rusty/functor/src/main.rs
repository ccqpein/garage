struct A<T> {
    inner: T,
}

impl Functor<i32, i32> for A<i32> {
    fn fmap(&self, f: &dyn Fn(i32) -> i32) -> i32 {
        f(self.inner)
    }
}

trait Functor<T1, T2> {
    fn fmap(&self, f: &dyn Fn(T1) -> T2) -> T2;
}

fn main() {}
