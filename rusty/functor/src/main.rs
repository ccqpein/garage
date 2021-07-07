struct A<T> {
    inner: T,
}

// method 1:
// impl Functor<i32, i32> for A<i32> {
//     fn fmap(&self, f: &dyn Fn(i32) -> i32) -> i32 {
//         f(self.inner)
//     }
// }

// trait Functor<T1, T2> {
//     fn fmap(&self, f: &dyn Fn(T1) -> T2) -> T2;
// }

// method 2:
// impl Functor for A<i32> {
//     type T = i32;
//     fn fmap<F, B>(&self, f: F) -> B
//     where
//         F: Fn(i32) -> B,
//     {
//         f(self.inner)
//     }
// }

// trait Functor {
//     type T;
//     fn fmap<F, B>(&self, f: F) -> B
//     where
//         F: Fn(Self::T) -> B;
// }

// method 3
trait Functor {
    type T;
    fn fmap<'a, F, B>(&'a self, f: F) -> B
    where
        F: Fn(&'a Self::T) -> B;
}

impl<S> Functor for A<S> {
    type T = S;
    fn fmap<'a, F, B>(&'a self, f: F) -> B
    where
        F: Fn(&'a S) -> B,
    {
        f(&self.inner)
    }
}

fn main() {
    let a = A { inner: 2 };
    assert_eq!(a.fmap(|x| *x + 2), 4);
}
