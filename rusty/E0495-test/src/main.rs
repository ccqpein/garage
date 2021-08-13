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
    data: &'a mut HashMapWrap,
}

impl<'a> Iterator for HashMapWrapIterMut<'a> {
    type Item = LeakResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(LeakResult {
            v: unsafe {
                (self.data.get_mut(&self.temp_k).unwrap() as *mut i32)
                    .as_mut()
                    .unwrap()
            },
        })
    }
}

struct LeakResult<'a> {
    v: &'a mut i32,
}

fn main() {}
