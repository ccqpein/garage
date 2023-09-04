use std::{cell::RefCell, collections::VecDeque, time::Duration};

use leptos::{html::Canvas, leptos_dom::console_log, *};
use wasm_bindgen::JsCast;
use web_sys::CanvasRenderingContext2d;

#[derive(Clone)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone)]
struct Snake {
    body: VecDeque<(u32, u32)>,
    dir: Direction,
}

impl Snake {
    fn new() -> Self {
        Self {
            body: vec![(15, 15), (14, 15)].into(),
            dir: Direction::Right,
        }
    }

    fn init_draw(&self, b: &CanvasRenderingContext2d) {
        for p in &self.body {
            self.draw_board(b, *p, "#000000")
        }
    }

    fn one_step_move(&mut self, food: &(u32, u32), b: &CanvasRenderingContext2d) {
        let head = self.body.get(0).unwrap().clone();

        match self.dir {
            Direction::Up => {
                if head.1 - 1 == food.1 {
                    self.body.push_front((head.0, head.1 - 1));
                    self.draw_board(b, (head.0, head.1 - 1), "#000000");
                //:= random pick the food
                } else {
                    self.body.push_front((head.0, head.1 - 1));
                    self.draw_board(b, (head.0, head.1 - 1), "#000000");

                    let tail = self.body.pop_back().unwrap();
                    self.draw_board(b, (tail.0, tail.1 - 1), "#CCCCCC")
                }
            }
            Direction::Down => {
                if head.1 + 1 == food.1 {
                    self.body.push_front((head.0, head.1 + 1));
                    self.draw_board(b, (head.0, head.1), "#000000");
                //:= random pick the food
                } else {
                    self.body.push_front((head.0, head.1 + 1));
                    self.draw_board(b, (head.0 + 1, head.1), "#000000");

                    let tail = self.body.pop_back().unwrap();
                    self.draw_board(b, (tail.0, tail.1), "#CCCCCC")
                }
            }
            Direction::Left => {
                if head.0 - 1 == food.0 {
                    self.body.push_front((head.0 - 1, head.1));
                    self.draw_board(b, (head.0 - 1, head.1), "#000000");
                //:= random pick the food
                } else {
                    self.body.push_front((head.0 - 1, head.1));
                    self.draw_board(b, (head.0 + 1, head.1), "#000000");

                    let tail = self.body.pop_back().unwrap();
                    self.draw_board(b, (tail.0, tail.1), "#CCCCCC");
                }
            }
            Direction::Right => {
                if head.0 + 1 == food.0 {
                    self.body.push_front((head.0 + 1, head.1));

                    self.draw_board(b, (head.0 + 1, head.1), "#000000");
                //:= random pick the food
                } else {
                    self.body.push_front((head.0 + 1, head.1));
                    self.draw_board(b, (head.0 + 1, head.1), "#000000");

                    let tail = self.body.pop_back().unwrap();
                    self.draw_board(b, (tail.0, tail.1), "#CCCCCC");
                }
            }
        }
    }

    fn draw_board(&self, b: &CanvasRenderingContext2d, pos: (u32, u32), color: &str) {
        //console_log(&format!("{}, {}", &pos.0, &pos.1));
        b.set_fill_style(&color.into());
        b.fill_rect(20.0 * pos.0 as f64, 20.0 * pos.1 as f64, 19.0, 19.0)
    }
}

#[component]
pub fn SnakeGame(cx: Scope, width: u32, height: u32) -> impl IntoView {
    let canvas_node = create_node_ref::<Canvas>(cx);

    let s = RefCell::new(Snake::new());
    let s1 = s.clone();
    // draw board
    canvas_node.on_load(cx, move |canvas_ref| {
        canvas_ref.set_width(width * 20);
        canvas_ref.set_height(height * 20);
        canvas_ref.on_mount(move |x| {
            // get access to the canvas
            let ctx = x
                .get_context("2d")
                .ok()
                .flatten()
                .expect("")
                .unchecked_into::<web_sys::CanvasRenderingContext2d>();
            ctx.set_fill_style(&"#CCCCCC".into());

            // draw rectangle on the canvas at the position (x,y) and provide width and height
            make_board(&ctx, width, height);
            s1.borrow().init_draw(&ctx);
        });
    });

    // refresh
    use_interval(cx, 1000, move || {
        let ctx = canvas_node
            .get()
            .unwrap()
            .get_context("2d")
            .ok()
            .flatten()
            .expect("")
            .unchecked_into::<web_sys::CanvasRenderingContext2d>();
        s.borrow_mut().one_step_move(&(30, 30), &ctx);
    });

    view! { cx,
            <p>hello</p>
            <canvas id="snake" node_ref=canvas_node></canvas>
    }
}

fn make_board(b: &web_sys::CanvasRenderingContext2d, row: u32, col: u32) {
    for r in 0..row {
        for c in 0..col {
            b.fill_rect(20.0 * c as f64, 20.0 * r as f64, 19.0, 19.0);
            //b.clear_rect(10.0 * c as f64, 10.0 * r as f64, 9.0, 9.0);
        }
    }
}

fn use_interval<F>(cx: Scope, interval_millis: u64, f: F)
where
    F: Fn() + Clone + 'static,
{
    create_effect(cx, move |prev_handle: Option<IntervalHandle>| {
        if let Some(prev_handle) = prev_handle {
            prev_handle.clear();
        };
        console_log("hello"); // just run once

        // here, we return the handle
        set_interval_with_handle(f.clone(), Duration::from_millis(interval_millis))
            .expect("could not create interval")
    });
}
