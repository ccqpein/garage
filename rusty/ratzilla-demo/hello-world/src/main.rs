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

use wasm_bindgen::JsValue;
use web_sys::console; // To convert Rust data to JavaScript values

fn main() -> io::Result<()> {
    //let counter = Rc::new(RefCell::new(0));
    //let backend = CanvasBackend::new()?;

    let backend = CanvasBackend::new_with_size(1210, 779)?;
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

    let mut rng = rand::rng();
    terminal.draw_web(move |f| {
        let size = f.area();

        let message = format!("f.area() size: {:?}", size);
        // f.area() size: Rect { x: 0, y: 0, width: 119, height: 35 }
        //console::log_1(&JsValue::from_str(&message)); // Log the string to console

        // 12 * 8
        for y in (0..size.height).step_by(5) {
            for x in (0..size.width).step_by(10) {
                let character = format!("{}, {}", x, y);
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
                    Rect::new(x, y, 10, 5),
                    //Rect::new(x, y, 10, 10),
                );

                // if rng.random_bool(0.2) {
                //     // 20% chance to draw a litter block
                //     // let character = match rng.random_range(0..4) {
                //     //     0 => "█",
                //     //     1 => "▒",
                //     //     2 => "░",
                //     //     _ => "#",
                //     // };

                //     let character = format!("{}, {}", x, y);
                //     // let color = match rng.random_range(0..5) {
                //     //     0 => Color::Red,
                //     //     1 => Color::Green,
                //     //     2 => Color::Blue,
                //     //     3 => Color::Yellow,
                //     //     _ => Color::DarkGray,
                //     // };

                //     let block = Block::default().style(Style::default().fg(Color::Yellow));

                //     f.render_widget(
                //         Paragraph::new(character)
                //             .alignment(Alignment::Center)
                //             .block(block),
                //         Rect::new(x, y, 10, 10),
                //         //Rect::new(x, y, 10, 10),
                //     );
                // }
            }
        }
    });

    Ok(())
}
