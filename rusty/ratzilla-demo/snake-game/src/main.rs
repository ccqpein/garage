use std::io;

use std::thread;
use std::time::Duration;

use rand::Rng;
use rand::rngs::ThreadRng;
use ratzilla::{
    CanvasBackend,
    ratatui::{
        Frame, Terminal,
        layout::{Alignment, Rect},
        style::{Color, Style},
        widgets::{Block, Paragraph},
    },
};

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
    rng: &mut ThreadRng,
) {
    // the size should sync with the snake limit
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

    let msg = format!(
        "{each_height}, {each_width}, {x_start}, {y_start}, {:?}\n",
        f.area(),
    );

    // 4, 8, 8, 4, Rect { x: 0, y: 0, width: 120, height: 40 }
    //console::log_1(&JsValue::from_str(&msg));
    // for (x, y) in snake.body() {
    //     f.render_widget(Block::new(), Rect::new(*x, *y, 1, 1));
    // }

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
        args.as_mut().unwrap().init_board_yet = true
    }

    //:= need the timer here

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
    snake.one_step(&(0, 0));

    // for x in 0..snake.x_limit {
    //     for y in 0..snake.y_limit {
    //         let character = format!(
    //             "{}, {}",
    //             x * each_width + x_start,
    //             y * each_height + y_start
    //         );
    //         //console::log_1(&JsValue::from_str(&format!("{x}, {y}")));
    //         let color = match rng.random_range(0..5) {
    //             0 => Color::Red,
    //             1 => Color::Green,
    //             2 => Color::Blue,
    //             3 => Color::Yellow,
    //             _ => Color::DarkGray,
    //         };
    //         let block = Block::default().style(Style::default().bg(color));

    //         f.render_widget(
    //             Paragraph::new(character)
    //                 .alignment(Alignment::Center)
    //                 .block(block),
    //             Rect::new(
    //                 x * each_width + x_start,
    //                 y * each_height + y_start,
    //                 each_width,
    //                 each_height,
    //             ),
    //         );
    //     }
    // }
}

fn main() -> io::Result<()> {
    let mut rng = rand::rng();
    let backend = CanvasBackend::new_with_size(1210, 779)?;
    let terminal = Terminal::new(backend)?;

    let mut snake = SnakeWidget::new(12, 8).unwrap();
    let mut render_arg = None;

    terminal.draw_web(move |f| {
        render_board(f, &mut render_arg, &mut snake, &mut rng);
    });

    Ok(())
}
