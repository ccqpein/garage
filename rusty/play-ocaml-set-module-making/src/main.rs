trait OrderedType {
    fn compare(a: &Self, b: &Self) -> bool {
        false
    }
}

// let me assume the vec is some set
trait S {
    type Elt;
    fn add(&mut self, e: Self::Elt);
}

#[derive(Debug)]
struct SStruct<T> {
    e: Vec<T>,
}

impl<T> S for SStruct<T> {
    type Elt = T;
    fn add(&mut self, e: Self::Elt) {
        self.e.push(e)
    }
}

fn make<T>() -> SStruct<T>
where
    T: OrderedType,
{
    SStruct {
        e: Default::default(),
    }
}

impl OrderedType for i32 {}

fn main() {
    let mut sss = make::<i32>();
    sss.add(15);

    println!("{:?}", sss);
}
