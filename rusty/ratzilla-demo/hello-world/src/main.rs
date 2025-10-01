use std::{cell::RefCell, io, rc::Rc};

use rand::Rng;
use ratzilla::{
    CanvasBackend,
    ratatui::{
        Terminal,
        layout::{Alignment, Rect},
        style::{Color, Style},
        widgets::{Block, Paragraph},
    },
};

use ratzilla::{WebRenderer, event::KeyCode};

fn main() -> io::Result<()> {
    //let counter = Rc::new(RefCell::new(0));
    //let backend = CanvasBackend::new()?;
    let backend = CanvasBackend::new_with_size(1600, 900)?;
    let terminal = Terminal::new(backend)?;

    // terminal.on_key_event({
    //     let counter_cloned = counter.clone();
    //     move |key_event| {
    //         if key_event.code == KeyCode::Char(' ') {
    //             let mut counter = counter_cloned.borrow_mut();
    //             *counter += 1;
    //         }
    //     }
    // });

    // terminal.draw_web(move |f| {
    //     let counter = counter.borrow();
    //     f.render_widget(
    //         Paragraph::new(format!("Count: {counter}"))
    //             .alignment(Alignment::Center)
    //             .block(
    //                 Block::bordered()
    //                     .title("Ratzilla")
    //                     .title_alignment(Alignment::Center)
    //                     .border_style(Color::Yellow),
    //             ),
    //         f.area(),
    //     );
    // });

    terminal.draw_web(move |f| {
        let size = f.area();
        let mut rng = rand::rng();

        for y in 0..size.height {
            for x in 0..size.width {
                if rng.random_bool(0.2) {
                    // 20% chance to draw a litter block
                    let character = match rng.random_range(0..4) {
                        0 => "█",
                        1 => "▒",
                        2 => "░",
                        _ => "#",
                    };
                    let color = match rng.random_range(0..5) {
                        0 => Color::Red,
                        1 => Color::Green,
                        2 => Color::Blue,
                        3 => Color::Yellow,
                        _ => Color::DarkGray,
                    };

                    let block = Block::default().style(Style::default().fg(color));

                    f.render_widget(
                        Paragraph::new(character)
                            .alignment(Alignment::Center)
                            .block(block),
                        Rect::new(x, y, 1, 1),
                    );
                }
            }
        }
    });

    Ok(())
}
