use std::io;

use rand::rngs::ThreadRng;
use ratzilla::{
    CanvasBackend,
    ratatui::{Frame, Terminal, layout::Rect, widgets::Block},
};
use ratzilla::{WebRenderer, event::KeyCode};

mod snake;
use snake::*;

fn render_board(f: &mut Frame, snake: &mut SnakeWidget, rng: &mut ThreadRng) {
    // the size should sync with the snake limit
    let size = f.area();
    for (x, y) in snake.body() {
        f.render_widget(Block::new(), Rect::new(*x, *y, 1, 1));
    }
}

fn main() -> io::Result<()> {
    let mut rng = rand::rng();
    let backend = CanvasBackend::new_with_size(1210, 779)?;
    let terminal = Terminal::new(backend)?;

    let mut rng = rand::rng();
    let mut snake = SnakeWidget::new(0, 0)?;
    terminal.draw_web(move |f| {
        let size = f.area();
        if snake.row_limit == 0 {
            snake.row_limit = size.height;
            snake.col_limit = size.width;
        }

        let mut snake = SnakeWidget::new(size.height); //:= need some tricks to get the size
    });

    Ok(())
}
