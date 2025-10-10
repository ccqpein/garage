use std::io;

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

fn render_board(f: &mut Frame, snake: &mut SnakeWidget, rng: &mut ThreadRng) {
    // the size should sync with the snake limit
    let size = f.area();
    let y_all = size.height;
    let x_all = size.width;

    let each_height = y_all / snake.row_limit + 2;
    let each_width = x_all / snake.col_limit + 2;
    let msg = format!("{each_height}, {each_width}, {:?}\n", size.clone());
    console::log_1(&JsValue::from_str(&msg));
    // for (x, y) in snake.body() {
    //     f.render_widget(Block::new(), Rect::new(*x, *y, 1, 1));
    // }

    // init the board
    for x in (each_width..x_all - each_width).step_by(each_width.into()) {
        for y in (each_height..x_all - each_height).step_by(each_height.into()) {
            let character = format!("{}, {}", x, y);
            //console::log_1(&JsValue::from_str(&msg));
            let color = match rng.random_range(0..5) {
                0 => Color::Red,
                1 => Color::Green,
                2 => Color::Blue,
                3 => Color::Yellow,
                _ => Color::DarkGray,
            };
            let block = Block::default().style(Style::default().bg(color));

            f.render_widget(
                Paragraph::new(character)
                    .alignment(Alignment::Center)
                    .block(block),
                Rect::new(x, y, each_width, each_height),
            );
        }
    }
}

fn main() -> io::Result<()> {
    let mut rng = rand::rng();
    let backend = CanvasBackend::new_with_size(1210, 779)?;
    let terminal = Terminal::new(backend)?;

    // let mut rng = rand::rng();
    let mut snake = SnakeWidget::new(12, 8).unwrap();
    terminal.draw_web(move |f| {
        render_board(f, &mut snake, &mut rng);
    });

    Ok(())
}
