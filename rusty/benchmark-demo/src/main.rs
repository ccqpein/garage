use cl_format::*;

fn main() {
    for _ in 0..100 {
        println!(
            "{}",
            cl_format!(
                "~@{~#[empty~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~:}",
                &1,
                &2,
                &3,
                &4,
                &5
            )
            .unwrap()
        )
    }
}
