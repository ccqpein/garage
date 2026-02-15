use crabtime;

// check the issue => https://github.com/wdanilo/crabtime/issues/34
// const MY_NUM: usize = crabtime::eval! {
//     (std::f32::consts::PI.sqrt() * 10.0).round() as usize
// };

#[crabtime::function]
#[macro_export] // <- This is how you export it!
fn gen_positions1() -> &str {
    "
    enum Position1 { X }
    enum Position2 { X, Y }
    enum Position3 { X, Y, Z }
    enum Position4 { X, Y, Z, W }
    "
}

gen_positions1!();

#[crabtime::expression]
fn gen_expr() {
    let output_num = 3;
    crabtime::output! {
        {{output_num}}
    }
}

#[crabtime::function]
fn gen_expr_2() {
    let output_num = 3;
    crabtime::output! {
        {{output_num}}
    }
}

fn test() {
    let x = gen_expr!(); // expression
    let x = {
        gen_expr_2!(); // function
    };
}

#[crabtime::function]
fn gen_positions4(components: Vec<String>) -> String {
    (1..=components.len())
        .map(|dim| {
            let cons = components[0..dim].join(",");
            format!("enum Position{dim} {{ {cons} }}")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[crabtime::function]
fn gen_positions7(name: String, components: Vec<String>) {
    for dim in 1..=components.len() {
        let cons = components[0..dim].join(",");
        crabtime::output! {
            enum {{name}}{{dim}} {
                {{cons}}
            }
        }
    }
}

// this one is weird
#[crabtime::function]
fn gen_positions8(pattern!($name:ident, $components:tt): _) {
    let components = expand!($components);
    for dim in 1..=components.len() {
        let cons = components[0..dim].join(",");
        // We don't need to use `expand!` here.
        let name = stringify!($name);
        crabtime::output! {
            enum {{name}}{{dim}} {
                {{cons}}
            }
        }
    }
}

fn main() {
    //println!("one-shot eval {MY_NUM}");
    let _ = Position1::X;
}
