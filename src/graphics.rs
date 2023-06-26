//! The frontend implementation for the game
//!
//! To get started, check out the [`main`] function

use catppuccin::{Colour, Flavour};
use sdl2::{event::Event, image::InitFlag, keyboard::Keycode, mouse::MouseButton, render::Canvas};

use crate::render::{Bot, GameRenderer};

/// The visible width of a single square
pub const WIDTH: u32 = 80;

/// A trait to draw something
pub trait Draw {
    /// how the thing should be drawn
    fn draw(&self, canvas: &mut Canvas<sdl2::video::Window>) -> Result<bool, String>;
}

/// Extension trait for various color formats
pub trait AsRgba {
    /// turns the color into a rgba representation
    fn as_rgba(&self) -> [f32; 4];
    /// turns the color into a sdl2 native [`Color`]
    ///
    /// [`Color`]: [`sdl2::pixels::Color`]
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

/// A wrapper for [`Strings`] representing errors. This is because sdl2 uses [String] for many
/// errors in its API
///
/// [`Strings`]: [String]
#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct StrError(String);

/// a main game loop for running the game
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

    if let Some(pos) = std::env::args().nth(2) {
        match pos.to_lowercase().as_str() {
            "b" | "black" => game.set_bot(Bot::Black),
            "w" | "white" => game.set_bot(Bot::White),
            "bw" | "wb" | "both" => game.set_bot(Bot::Both),
            "-" | "n" | "neither" => game.set_bot(Bot::Neither),
            x => panic!("invalid option {x}"),
        }
    }

    if let Some(pos) = std::env::args().nth(3) {
        game.set_position(&pos)?;
    }

    canvas.set_draw_color(flavor.base().as_sdl());
    canvas.clear();
    game.draw(&mut canvas).map_err(StrError)?;
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut draws = 0usize;
    let mut all = 0usize;
    'running: loop {
        let mut did_change = false;
        game.make_bot_move();

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
        }
        game.check_outcome();

        did_change = did_change || game.draw(&mut canvas).map_err(StrError)?;

        if did_change {
            draws += 1;
            canvas.present();
        }
        all += 1;
    }

    eprintln!("drew {draws} out of {all} times");

    Ok(())
}

// pub struct Store<R: Resources> {
//     textures: [Texture<R>; 12],
// }
