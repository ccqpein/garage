use std::collections::HashMap;

struct HashMapWrap {
    table: HashMap<i32, i32>,
}

impl<'a> HashMapWrap {
    fn get_mut(&mut self, k: &i32) -> Option<&mut i32> {
        self.table.get_mut(k)
    }
}

struct HashMapWrapIterMut<'a> {
    temp_k: i32,
    //data: &'a mut HashMapWrap,
    data: Option<&'a mut HashMapWrap>,
}

// impl<'a> Iterator for HashMapWrapIterMut<'a> {
//     fn next(&mut self) -> Option<Self::Item> {
//         Some(LeakResult {
//             v: unsafe {
//                 (self.data.get_mut(&self.temp_k).unwrap() as *mut i32)
//                     .as_mut()
//                     .unwrap()
//             },
//         })
//     }
// }

impl<'a> Iterator for HashMapWrapIterMut<'a> {
    type Item = LeakResult<'a>;

    // get solution in
    // https://users.rust-lang.org/t/need-some-help-of-e495/63507/9
    fn next(&mut self) -> Option<Self::Item> {
        self.temp_k += 1;
        match self.data.take() {
            Some(data) => Some(LeakResult {
                v: data.get_mut(&self.temp_k).unwrap(),
            }),
            None => None,
        }
    }
}

struct LeakResult<'a> {
    v: &'a mut i32,
}

fn main() {}
