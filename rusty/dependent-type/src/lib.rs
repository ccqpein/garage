trait Succ {
    //type T: Succ;
    type N: Succ;
    type L: Succ;

    fn succ(self) -> Self::N;
    fn before(self) -> Self::L;
}

// enum Nat{
//     Zero,
//     S(Nat),
// }

// impl Succ for Nat{
//     type N = Nat;
//     fn succ(self) -> Nat {
//         Nat::S(self)
//     }
// }

// fn use_s(s: &impl Succ) {}
// fn test() {
//     let a = Nat<_>::Zero;
// }

#[derive(Clone, PartialEq)]
struct Zero;

impl Succ for Zero {
    type N = S<Zero>;
    //type T = Zero;
    type L = Zero;
    fn succ(self) -> S<Self> {
        S(self.clone())
    }

    fn before(self) -> Zero {
        self
    }

    // fn last(self) -> Zero {
    //     self
    // }
}

#[derive(Clone, Copy)]
struct S<T: Succ>(T);

impl<Z: Succ> Succ for S<Z> {
    type N = S<S<Z>>;
    //type T = Self;
    type L = Z;

    fn succ(self) -> S<Self> {
        S(self)
    }

    fn before(self) -> Self::L {
        self.0
    }

    // fn last(self) -> <Z as Succ>::T {
    //     self.0
    // }
}

struct SS<T> {
    inner: Option<T>,
}

fn use_type_check<T: Succ + std::cmp::PartialEq>(s: S<T>) -> usize {
    let mut count = 0;
    while s.before() != Zero {
        count += 1;
    }
    count
}

fn ss_test() {
    let z: SS<i32> = SS { inner: Some(0) };
    let o = SS { inner: Some(z) };
    print_type_of(&o);

    let z1 = Zero;
    let o1 = z1.succ().succ();
    print_type_of(&o1);
}

fn use_s(s: impl Succ) {}
fn use_ss(s: SS<i32>) {}

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_succ_type() {
        //let z = Zero { count: 0 };
        //print_type_of(&z.succ().succ());

        ss_test()
    }

    #[test]
    fn test_use_in_function() {
        let z: SS<i32> = SS { inner: Some(0) };
        let o = SS { inner: Some(z) };
        //use_ss(o);
        let z1 = Zero;
        let o1 = z1.succ().succ();
        use_s(o1);

        let z1 = Zero;
        let o1 = z1.succ().succ();
        let o2 = o1.before().before();
        print_type_of(&o2);
    }
}
