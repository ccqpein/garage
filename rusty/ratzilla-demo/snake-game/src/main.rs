use ratzilla::{
    CanvasBackend,
    ratatui::{
        Frame, Terminal,
        layout::{Alignment, Rect},
        style::{Color, Style},
        widgets::{Block, Paragraph},
    },
};
use std::{cell::RefCell, io, rc::Rc};

use ratzilla::{WebRenderer, event::KeyCode};

mod snake;
use snake::*;

use wasm_bindgen::JsValue;
use web_sys::console;

/// just the wrapper of rendor arguments
struct RenderArgs {
    x_start: u16,
    y_start: u16,

    each_width: u16,
    each_height: u16,

    init_board_yet: bool,
}

/// get the range, return the start point and the width
fn range_make(v: u16, div: u16) -> (u16, u16) {
    if v % div == 0 {
        (v / (div + 2), v / (div + 1))
    } else {
        let mod_v = v % div;
        ((mod_v / 2), (v - mod_v) / div)
    }
}

fn render_board(
    f: &mut Frame,
    args: &mut Option<RenderArgs>,
    snake: &mut SnakeWidget,
    food: &mut (u16, u16),
) -> Option<String> {
    if args.is_none() {
        let size = f.area();
        let y_all = size.height;
        let x_all = size.width;

        let (x_start, each_width) = range_make(x_all, snake.x_limit);
        let (y_start, each_height) = range_make(y_all, snake.y_limit);

        *args = Some(RenderArgs {
            x_start,
            y_start,
            each_width,
            each_height,
            init_board_yet: false,
        })
    }

    let x_start = args.as_ref().unwrap().x_start;
    let y_start = args.as_ref().unwrap().y_start;
    let each_width = args.as_ref().unwrap().each_width;
    let each_height = args.as_ref().unwrap().each_height;

    // 4, 8, 8, 4, Rect { x: 0, y: 0, width: 120, height: 40 }
    // let msg = format!(
    //     "{each_height}, {each_width}, {x_start}, {y_start}, {:?}\n",
    //     f.area(),
    // );

    // init the board
    if !args.as_ref().unwrap().init_board_yet {
        for x in 0..snake.x_limit {
            for y in 0..snake.y_limit {
                let color = Color::DarkGray;
                let block = Block::default().style(Style::default().bg(color));

                f.render_widget(
                    block,
                    Rect::new(
                        x * each_width + x_start,
                        y * each_height + y_start,
                        each_width,
                        each_height,
                    ),
                );
            }
        }

        *food = if let Some(f) = snake.new_food() {
            f
        } else {
            return Some("full board".to_string());
        };

        args.as_mut().unwrap().init_board_yet = true
    }

    for (x, y) in snake.body() {
        // console::log_1(&JsValue::from_str(&format!(
        //     "{:?}",
        //     snake.body().collect::<Vec<_>>()
        // )));
        let color = Color::DarkGray;
        let block = Block::default().style(Style::default().bg(color));

        f.render_widget(
            block,
            Rect::new(
                x * each_width + x_start,
                y * each_height + y_start,
                each_width,
                each_height,
            ),
        );
    }

    f.render_widget(
        Block::default().style(Style::default().bg(Color::Red)),
        Rect::new(
            food.0 * each_width + x_start,
            food.1 * each_height + y_start,
            each_width,
            each_height,
        ),
    );

    match snake.one_step(food) {
        Ok(status) => match status {
            Status::Eaten => {
                *food = match snake.new_food() {
                    Some(f) => f,
                    None => return Some("You win!".to_string()),
                };

                f.render_widget(
                    Block::default().style(Style::default().bg(Color::Red)),
                    Rect::new(
                        food.0 * each_width + x_start,
                        food.1 * each_height + y_start,
                        each_width,
                        each_height,
                    ),
                );
            }
            Status::Normal => (),
            Status::Lose => return Some("You lose!".to_string()),
        },
        Err(e) => return Some(format!("ERROR: {e}")),
    }

    None
}

fn main() -> io::Result<()> {
    let backend = CanvasBackend::new_with_size(1210, 779)?;
    let terminal = Terminal::new(backend)?;

    let dir = Rc::new(RefCell::new(Dir::Up));
    let mut food = (0, 0);

    
    let mut snake = SnakeWidget::new(6, 6, Rc::clone(&dir)).unwrap();
    let mut render_arg = None;

    terminal.on_key_event({
        move |key_event| match key_event.code {
            KeyCode::Left => {
                if !dir.borrow().is_right() {
                    *dir.borrow_mut() = Dir::Left;
                }
            }
            KeyCode::Right => {
                if !dir.borrow().is_left() {
                    *dir.borrow_mut() = Dir::Right;
                }
            }
            KeyCode::Up => {
                if !dir.borrow().is_down() {
                    *dir.borrow_mut() = Dir::Up;
                }
            }
            KeyCode::Down => {
                if !dir.borrow().is_up() {
                    *dir.borrow_mut() = Dir::Down;
                }
            }
            KeyCode::Esc => todo!(),
            _ => (),
        }
    });

    terminal.draw_web(move |f| {
        if let Some(msg) = render_board(f, &mut render_arg, &mut snake, &mut food) {
            
            //console::log_1(&JsValue::from_str(&msg));
        }
    });

    Ok(())
}
