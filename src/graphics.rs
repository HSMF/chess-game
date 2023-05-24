use catppuccin::{Colour, Flavour};
use sdl2::{event::Event, image::InitFlag, keyboard::Keycode, mouse::MouseButton, render::Canvas};

use crate::game::GameRenderer;

pub const WIDTH: u32 = 80;

pub trait Draw {
    fn draw(&self, canvas: &mut Canvas<sdl2::video::Window>) -> Result<(), String>;
}

pub trait AsRgba {
    fn as_rgba(&self) -> [f32; 4];
    fn as_sdl(&self) -> sdl2::pixels::Color;
}

impl AsRgba for Colour {
    fn as_rgba(&self) -> [f32; 4] {
        let (r, g, b) = (*self).into();
        [r as f32 / 256.0, g as f32 / 256.0, b as f32 / 256.0, 1.0]
    }

    fn as_sdl(&self) -> sdl2::pixels::Color {
        let (r, g, b) = (*self).into();

        sdl2::pixels::Color {
            r,
            g,
            b,
            a: u8::MAX,
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct StrError(String);

pub fn main() -> anyhow::Result<()> {
    let flavor = Flavour::Mocha;

    let sdl_context = sdl2::init().map_err(StrError)?;
    let video_subsystem = sdl_context.video().map_err(StrError)?;

    let _image_context = sdl2::image::init(InitFlag::PNG | InitFlag::JPG).map_err(StrError)?;

    let window = video_subsystem
        .window("chees", WIDTH * 8, WIDTH * 8)
        .position_centered()
        .build()?;

    let id = window.id();

    let mut canvas = window.into_canvas().software().build()?;

    let texture_creator = canvas.texture_creator();

    let mut game = GameRenderer::new(&texture_creator).map_err(StrError)?;

    canvas.set_draw_color(flavor.base().as_sdl());
    canvas.clear();
    game.draw(&mut canvas).map_err(StrError)?;
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                Event::MouseButtonDown {
                    mouse_btn: MouseButton::Left,
                    window_id,
                    clicks,
                    x,
                    y,
                    ..
                } if window_id == id && clicks % 2 == 1 && x > 0 && y > 0 => {
                    game.select_square(x as u32, y as u32);
                }
                Event::MouseButtonDown {
                    mouse_btn: MouseButton::Left,
                    window_id,
                    clicks,
                    ..
                } if window_id == id && clicks % 2 == 0 => {
                    game.deselect_square();
                }
                _ => {}
            }

            game.draw(&mut canvas).map_err(StrError)?;
            canvas.present();
        }
    }

    Ok(())
}

// pub struct Store<R: Resources> {
//     textures: [Texture<R>; 12],
// }
