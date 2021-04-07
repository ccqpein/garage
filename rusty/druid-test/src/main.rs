use druid::{
    widget::{Align, Flex, Label, Padding},
    BoxConstraints, Color, Env, Event, EventCtx, LayoutCtx, LifeCycle, LifeCycleCtx, PaintCtx,
    Point, RenderContext, UpdateCtx,
};
use druid::{AppLauncher, Data, PlatformError, Rect, Size, Widget, WindowDesc};

#[derive(Clone, Data)]
struct AppData {}

struct Square {
    cell_num_row: usize,
    cell_num_col: usize,
    cell_size: f64,
}

/// have to impl widget for druid
impl Widget<AppData> for Square {
    fn event(&mut self, ctx: &mut EventCtx<'_, '_>, event: &Event, data: &mut AppData, env: &Env) {}

    fn lifecycle(
        &mut self,
        ctx: &mut LifeCycleCtx<'_, '_>,
        event: &LifeCycle,
        data: &AppData,
        env: &Env,
    ) {
    }

    fn update(
        &mut self,
        ctx: &mut UpdateCtx<'_, '_>,
        old_data: &AppData,
        data: &AppData,
        env: &Env,
    ) {
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
                ctx.fill(rect, &Color::rgb8(0x0, row as u8, col as u8))
            }
        }
    }
}

fn make_canvas() -> impl Widget<AppData> {
    Flex::column().with_flex_child(
        Flex::row().with_flex_child(
            Flex::column().with_flex_child(
                Square {
                    cell_num_col: 100,
                    cell_num_row: 10,
                    cell_size: 10.,
                },
                1.0,
            ),
            1.0,
        ),
        1.0,
    )
}

fn main() -> Result<(), PlatformError> {
    AppLauncher::with_window(WindowDesc::new(make_canvas())).launch(AppData {})?;
    Ok(())
}
