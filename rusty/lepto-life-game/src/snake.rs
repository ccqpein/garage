//:= need more doc
//:= need to know how to deploy
use rand::rngs::ThreadRng;
use rand::seq::SliceRandom;
use std::{
    cell::RefCell,
    collections::{HashSet, VecDeque},
    rc::Rc,
    time::Duration,
};

use leptos::{html::Canvas, leptos_dom::console_log, *};
use wasm_bindgen::JsCast;
//use web_sys::CanvasRenderingContext2d;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone)]
struct Snake {
    body: VecDeque<(u32, u32)>,
    width: u32,
    height: u32,
    food_farm: HashSet<(u32, u32)>,
    dir: Direction,
    canvas: NodeRef<Canvas>,

    // random
    rng: ThreadRng,
}

impl Snake {
    fn new(width: u32, height: u32, canvas: NodeRef<Canvas>) -> Result<Self, String> {
        if width < 2 || height < 2 {
            return Err("too small width or height".to_string());
        }

        let head = (width / 2, height / 2);
        let tail = (head.0 - 1, head.1);
        //console_log(&format!("{:?}", head));

        let mut food_farm = HashSet::new();
        for r in 0..height {
            for c in 0..width {
                food_farm.insert((c, r));
            }
        }

        Ok(Self {
            body: vec![head, tail].into(),
            dir: Direction::Right,
            food_farm,
            width,
            height,
            canvas,
            rng: rand::thread_rng(),
        })
    }

    fn init_draw(&self) {
        for p in &self.body {
            self.draw_board(*p, "#000000")
        }
    }

    fn pick_food(&mut self) -> Option<(u32, u32)> {
        let snake_body = self.body.iter().cloned().collect::<HashSet<_>>();
        let slice = self.food_farm.difference(&snake_body).collect::<Vec<_>>();
        slice.choose(&mut self.rng).cloned().copied()
    }

    fn in_body(&self, point: &(u32, u32)) -> bool {
        self.body.contains(point)
    }

    fn one_step_move(&mut self, food: &(u32, u32)) -> (Option<(u32, u32)>, bool, bool) {
        // new food and dead or not
        let head = self.body.get(0).unwrap().clone();
        //console_log(&format!("{:?}", self.dir));
        match self.dir {
            Direction::Up => {
                if head.1 == 0 {
                    return (None, true, false);
                }

                if head.1 - 1 == food.1 && head.0 == food.0 {
                    self.body.push_front((head.0, head.1 - 1));
                    self.draw_board((head.0, head.1 - 1), "#000000");
                    match self.pick_food() {
                        Some(f) => {
                            self.draw_board(f, "#f20505");
                            (Some(f), false, false)
                        }
                        None => (None, false, true),
                    }
                } else if self.in_body(&(head.0, head.1 - 1)) {
                    (None, true, false)
                } else {
                    self.body.push_front((head.0, head.1 - 1));
                    self.draw_board((head.0, head.1 - 1), "#000000");

                    let tail = self.body.pop_back().unwrap();
                    self.draw_board((tail.0, tail.1), "#CCCCCC");
                    (None, false, false)
                }
            }
            Direction::Down => {
                if head.1 + 1 == self.height {
                    return (None, true, false);
                }

                if head.1 + 1 == food.1 && head.0 == food.0 {
                    self.body.push_front((head.0, head.1 + 1));
                    self.draw_board((head.0, head.1 + 1), "#000000");
                    match self.pick_food() {
                        Some(f) => {
                            self.draw_board(f, "#f20505");
                            (Some(f), false, false)
                        }
                        None => (None, false, true),
                    }
                } else if self.in_body(&(head.0, head.1 + 1)) {
                    (None, true, false)
                } else {
                    self.body.push_front((head.0, head.1 + 1));
                    self.draw_board((head.0, head.1 + 1), "#000000");

                    let tail = self.body.pop_back().unwrap();
                    self.draw_board((tail.0, tail.1), "#CCCCCC");
                    (None, false, false)
                }
            }
            Direction::Left => {
                if head.0 == 0 {
                    return (None, true, false);
                }

                if head.0 - 1 == food.0 && head.1 == food.1 {
                    self.body.push_front((head.0 - 1, head.1));
                    self.draw_board((head.0 - 1, head.1), "#000000");
                    match self.pick_food() {
                        Some(f) => {
                            self.draw_board(f, "#f20505");
                            (Some(f), false, false)
                        }
                        None => (None, false, true),
                    }
                } else if self.in_body(&(head.0 - 1, head.1)) {
                    (None, true, false)
                } else {
                    self.body.push_front((head.0 - 1, head.1));
                    self.draw_board((head.0 - 1, head.1), "#000000");

                    let tail = self.body.pop_back().unwrap();
                    self.draw_board((tail.0, tail.1), "#CCCCCC");
                    (None, false, false)
                }
            }
            Direction::Right => {
                if head.0 + 1 == self.width {
                    return (None, true, false);
                }

                if head.0 + 1 == food.0 && head.1 == food.1 {
                    self.body.push_front((head.0 + 1, head.1));
                    self.draw_board((head.0 + 1, head.1), "#000000");
                    match self.pick_food() {
                        Some(f) => {
                            self.draw_board(f, "#f20505");
                            (Some(f), false, false)
                        }
                        None => (None, false, true),
                    }
                } else if self.in_body(&(head.0 + 1, head.1)) {
                    (None, true, false)
                } else {
                    self.body.push_front((head.0 + 1, head.1));
                    self.draw_board((head.0 + 1, head.1), "#000000");

                    let tail = self.body.pop_back().unwrap();
                    self.draw_board((tail.0, tail.1), "#CCCCCC");
                    (None, false, false)
                }
            }
        }
    }

    fn draw_board(&self, pos: (u32, u32), color: &str) {
        let b = self
            .canvas
            .get_untracked()
            .unwrap()
            .get_context("2d")
            .ok()
            .flatten()
            .expect("")
            .unchecked_into::<web_sys::CanvasRenderingContext2d>();
        b.set_fill_style(&color.into());
        b.fill_rect(20.0 * pos.0 as f64, 20.0 * pos.1 as f64, 19.0, 19.0)
    }

    fn change_direction(&mut self, dir: &u32) {
        match dir {
            119 if self.dir != Direction::Down => self.dir = Direction::Up,
            115 if self.dir != Direction::Up => self.dir = Direction::Down,
            97 if self.dir != Direction::Right => self.dir = Direction::Left,
            100 if self.dir != Direction::Left => self.dir = Direction::Right,
            _ => {}
        }
    }

    fn reset(&mut self, food: Rc<RefCell<(u32, u32)>>) {
        let head = (self.width / 2, self.height / 2);
        let tail = (head.0 - 1, head.1);
        self.body = vec![head, tail].into();

        self.dir = Direction::Right;

        let ctx = self
            .canvas
            .get()
            .unwrap()
            .get_context("2d")
            .ok()
            .flatten()
            .expect("")
            .unchecked_into::<web_sys::CanvasRenderingContext2d>();
        make_board(&ctx, self.width, self.height);
        self.init_draw();

        let new_food = self.pick_food();
        *food.borrow_mut() = new_food.unwrap();

        self.draw_board(new_food.unwrap(), "#f20505");
    }
}

#[component]
pub fn SnakeGame(cx: Scope, width: u32, height: u32) -> impl IntoView {
    let canvas_node = create_node_ref::<Canvas>(cx);

    let s = Rc::new(RefCell::new(
        Snake::new(width, height, canvas_node.clone()).unwrap(),
    ));
    let food = Rc::new(RefCell::new(s.borrow_mut().pick_food().unwrap()));

    let s1 = s.clone();
    let food1 = food.clone();
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

            // draw rectangle on the canvas at the position (x,y) and provide width and height
            make_board(&ctx, width, height);
            s1.borrow().init_draw();
            s1.borrow().draw_board(*food1.borrow(), "#f20505");
        });
    });

    let s2 = s.clone();
    window_event_listener(ev::keypress, move |ev| {
        s2.borrow_mut().change_direction(&ev.char_code());
    });

    // dead flag
    let (dead, set_dead) = create_signal(cx, false);

    // win flag
    let (win, set_win) = create_signal(cx, false);

    // refresh
    let food2 = food.clone();
    let s3 = s.clone();
    use_interval(
        cx,
        500,
        move || {
            //console_log(&format!("{:?}", food2.borrow()));

            let last_food = food2.borrow().clone();
            match s3.borrow_mut().one_step_move(&last_food) {
                (Some(f), _, _) => *food2.borrow_mut() = f,
                (None, true, _) => set_dead(true), //console_log("dead")
                (None, false, true) => {
                    set_win(true);
                    //set_dead(true);
                }
                _ => {}
            }
        },
        dead,
        win,
    );

    view! { cx,
        <StatusBar/>
        <Restart set_dead=set_dead set_win=set_win s=s food=food/>
        <br/>
        <canvas id="snake" node_ref=canvas_node></canvas>
        {move || {
            if dead() {
                leptos::window().alert_with_message("Dead, click restart").unwrap()
            }
        }}
        {move || {
            if win() {
                leptos::window().alert_with_message("You win! Click restart start new").unwrap()
            }
        }}
    }
}

#[component]
fn StatusBar(cx: Scope) -> impl IntoView {
    view! { cx, <p>"W A S D => Up Left Down Right"</p> }
}

#[component]
fn Restart(
    cx: Scope,
    set_dead: WriteSignal<bool>,
    set_win: WriteSignal<bool>,
    s: Rc<RefCell<Snake>>,
    food: Rc<RefCell<(u32, u32)>>,
) -> impl IntoView {
    view! { cx,
        <button on:click=move |_| {
            set_dead(false);
            set_win(false);
            s.borrow_mut().reset(food.clone());
        }>"Restart"</button>
    }
}

fn make_board(b: &web_sys::CanvasRenderingContext2d, row: u32, col: u32) {
    b.set_fill_style(&"#CCCCCC".into());
    for r in 0..row {
        for c in 0..col {
            b.fill_rect(20.0 * c as f64, 20.0 * r as f64, 19.0, 19.0);
        }
    }
}

fn use_interval<F>(
    cx: Scope,
    interval_millis: u64,
    f: F,
    stop: ReadSignal<bool>,
    win: ReadSignal<bool>,
) where
    F: Fn() + Clone + 'static,
{
    create_effect(
        cx,
        move |prev_handle: Option<IntervalHandle>| -> IntervalHandle {
            if stop() || win() {
                if let Some(prev_handle) = prev_handle {
                    prev_handle.clear();
                };
                prev_handle.unwrap()
            } else {
                if let Some(prev_handle) = prev_handle {
                    prev_handle.clear();
                };

                //console_log("hello"); // just run once

                // here, we return the handle
                set_interval_with_handle(f.clone(), Duration::from_millis(interval_millis))
                    .expect("could not create interval")
            }
        },
    );
}
