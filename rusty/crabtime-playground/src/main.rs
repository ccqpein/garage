use crabtime;

const MY_NUM: usize = crabtime::eval! {
    (std::f32::consts::PI.sqrt() * 10.0).round() as usize
};

fn main() {
    println!("one-shot eval {MY_NUM}");
}
