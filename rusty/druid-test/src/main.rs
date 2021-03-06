use druid::{
    text::{Attribute, RichText},
    widget::{Align, Flex, Label, Padding},
    BoxConstraints, Color, Env, Event, EventCtx, LayoutCtx, LifeCycle, LifeCycleCtx, PaintCtx,
    Point, RenderContext, UpdateCtx,
};
use druid::{AppLauncher, Data, PlatformError, Rect, Size, TimerToken, Widget, WindowDesc};
use std::time::{Duration, Instant};

#[derive(Clone, Data)]
struct AppData {
    color: u8,
}

struct Square {
    cell_num_row: usize,
    cell_num_col: usize,
    cell_size: f64,
    color: u8,
    timestamp: TimerToken,
}

/// have to impl widget for druid
impl Widget<AppData> for Square {
    fn event(&mut self, ctx: &mut EventCtx<'_, '_>, event: &Event, data: &mut AppData, env: &Env) {
        match event {
            Event::WindowConnected => {
                ctx.request_paint();
                self.timestamp = ctx.request_timer(Duration::from_secs(2));
            }
            Event::Timer(id) => {
                if *id == self.timestamp {
                    self.timestamp = ctx.request_timer(Duration::from_secs(2));
                    if data.color == 255 {
                        data.color = 0;
                    } else {
                        data.color = 255
                    }
                    //println!("in timer");
                    //ctx.request_paint()
                }
            }
            _ => {}
        }
    }

    fn lifecycle(
        &mut self,
        _ctx: &mut LifeCycleCtx<'_, '_>,
        _event: &LifeCycle,
        _data: &AppData,
        _env: &Env,
    ) {
    }

    fn update(
        &mut self,
        ctx: &mut UpdateCtx<'_, '_>,
        old_data: &AppData,
        data: &AppData,
        env: &Env,
    ) {
        println!("update");
        ctx.request_paint()
        // if data.timestamp - old_data.timestamp >= Duration::new(1, 0) {
        //     self.color = 252;
        //     ctx.request_paint();
        // }
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx<'_, '_>,
        bc: &BoxConstraints,
        data: &AppData,
        env: &Env,
    ) -> Size {
        let col = self.cell_num_col as f64 * self.cell_size;
        let row = self.cell_num_row as f64 * self.cell_size;
        Size::new(row, col)
    }

    fn paint(&mut self, ctx: &mut PaintCtx<'_, '_, '_>, data: &AppData, env: &Env) {
        let cell_size = Size {
            width: self.cell_size,
            height: self.cell_size,
        };
        for row in 0..self.cell_num_row {
            for col in 0..self.cell_num_col {
                let point = Point {
                    x: self.cell_size * row as f64,
                    y: self.cell_size * col as f64,
                };
                let rect = Rect::from_origin_size(point, cell_size);
                ctx.fill(rect, &Color::rgb8(data.color, row as u8, col as u8))
            }
        }

        //let label: Label<()> = Label::new("Center me");
        //let centered = Align::centered(label);
    }
}

fn make_canvas() -> impl Widget<AppData> {
    Flex::column().with_flex_child(
        Flex::row().with_flex_child(
            Square {
                cell_num_col: 100,
                cell_num_row: 10,
                cell_size: 10.,
                color: 0,
                timestamp: TimerToken::INVALID,
            },
            1.0,
        ),
        1.0,
    )
}

fn main() -> Result<(), PlatformError> {
    AppLauncher::with_window(WindowDesc::new(make_canvas())).launch(AppData { color: 0 })?;
    Ok(())
}
