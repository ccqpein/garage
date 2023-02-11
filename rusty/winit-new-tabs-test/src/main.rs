use std::collections::HashSet;

use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    platform::macos::WindowExtMacOS,
    window::{Fullscreen, Window, WindowBuilder},
};

fn main() {
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new()
        //.with_fullscreen(Some(Fullscreen::Borderless(None)))
        .build(&event_loop)
        .unwrap();

    //dbg!(&window.set_fullscreen(Fullscreen::Borderless(None)));
    let mut status = HashSet::new();

    event_loop.run(move |event, event_loop, control_flow| {
        *control_flow = ControlFlow::Wait;

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                window_id,
            } if window_id == window.id() => *control_flow = ControlFlow::Exit,

            Event::WindowEvent {
                event:
                    WindowEvent::KeyboardInput {
                        device_id,
                        input,
                        is_synthetic,
                    },
                window_id,
            } => {
                match &input.state {
                    winit::event::ElementState::Pressed if input.scancode == 17 => {
                        if status.contains(&54) {
                            println!("hello");
                            let monitor = event_loop
                                .available_monitors()
                                .next()
                                //.nth(1)
                                .expect("no monitor found!");

                            if window.fullscreen().is_some() {
                                window.set_fullscreen(None);
                            } else {
                                let fullscreen =
                                    Some(Fullscreen::Borderless(Some(monitor.clone())));
                                window.set_fullscreen(fullscreen);
                            }
                        }
                    }
                    winit::event::ElementState::Pressed => {
                        status.insert(input.scancode);
                    }
                    winit::event::ElementState::Released => {
                        status.remove(&input.scancode);
                    }
                    _ => (),
                };

                println!("input: {:?}", input);
            }
            _ => {
                //println!("{:?}", event);
            }
        }
    });
}
