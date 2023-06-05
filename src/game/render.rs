use catppuccin::Flavour;
use itertools::Itertools;
use sdl2::{
    image::LoadTexture,
    rect::Rect,
    render::{Texture, TextureCreator},
    video::WindowContext,
};

use crate::{
    graphics::{AsRgba, Draw},
    Piece,
};

use super::{Game, Position};

/// Store for the piece textures
pub(super) struct TextureStore<'a> {
    white_rook: Texture<'a>,
    white_knight: Texture<'a>,
    white_bishop: Texture<'a>,
    white_queen: Texture<'a>,
    white_king: Texture<'a>,
    white_pawn: Texture<'a>,
    black_rook: Texture<'a>,
    black_knight: Texture<'a>,
    black_bishop: Texture<'a>,
    black_queen: Texture<'a>,
    black_king: Texture<'a>,
    black_pawn: Texture<'a>,
}

impl<'a> TextureStore<'a> {
    pub(super) fn new(texture_creator: &'a TextureCreator<WindowContext>) -> Result<Self, String> {
        let black_bishop = texture_creator.load_texture("assets/bishop-black.png")?;
        let white_bishop = texture_creator.load_texture("assets/bishop-white.png")?;
        let black_king = texture_creator.load_texture("assets/king-black.png")?;
        let white_king = texture_creator.load_texture("assets/king-white.png")?;
        let black_knight = texture_creator.load_texture("assets/knight-black.png")?;
        let white_knight = texture_creator.load_texture("assets/knight-white.png")?;
        let black_pawn = texture_creator.load_texture("assets/pawn-black.png")?;
        let white_pawn = texture_creator.load_texture("assets/pawn-white.png")?;
        let black_queen = texture_creator.load_texture("assets/queen-black.png")?;
        let white_queen = texture_creator.load_texture("assets/queen-white.png")?;
        let black_rook = texture_creator.load_texture("assets/rook-black.png")?;
        let white_rook = texture_creator.load_texture("assets/rook-white.png")?;

        Ok(TextureStore {
            white_rook,
            white_knight,
            white_bishop,
            white_queen,
            white_king,
            white_pawn,
            black_rook,
            black_knight,
            black_bishop,
            black_queen,
            black_king,
            black_pawn,
        })
    }

    fn piece_texture(&self, piece: Piece) -> &Texture<'a> {
        match (piece.color, piece.kind) {
            (crate::Player::Black, crate::PieceKind::Pawn) => &self.black_pawn,
            (crate::Player::Black, crate::PieceKind::Rook) => &self.black_rook,
            (crate::Player::Black, crate::PieceKind::Knight) => &self.black_knight,
            (crate::Player::Black, crate::PieceKind::Bishop) => &self.black_bishop,
            (crate::Player::Black, crate::PieceKind::Queen) => &self.black_queen,
            (crate::Player::Black, crate::PieceKind::King) => &self.black_king,
            (crate::Player::White, crate::PieceKind::Pawn) => &self.white_pawn,
            (crate::Player::White, crate::PieceKind::Rook) => &self.white_rook,
            (crate::Player::White, crate::PieceKind::Knight) => &self.white_knight,
            (crate::Player::White, crate::PieceKind::Bishop) => &self.white_bishop,
            (crate::Player::White, crate::PieceKind::Queen) => &self.white_queen,
            (crate::Player::White, crate::PieceKind::King) => &self.white_king,
        }
    }
}

pub struct GameRenderer<'a> {
    store: TextureStore<'a>,
    game: Game,
    selected_square: Option<Position>,
}

impl<'a> GameRenderer<'a> {
    pub fn new(texture_creator: &'a TextureCreator<WindowContext>) -> Result<Self, String> {
        let store = TextureStore::new(texture_creator)?;
        Ok(GameRenderer {
            store,
            game: Game::new(),
            selected_square: None,
        })
    }

    pub fn select_square(&mut self, x: u32, y: u32) {
        let pos = Position::from_physical(x, y);

        if let Some(prev) = self.selected_square {
            if prev == pos {
                self.selected_square = None;
                return;
            }
            // match self.game.board[prev] {
            //     Some(piece) if piece.color == self.game.to_move => {
            //         self.
            //     }
            //     _ => {}
            // }
        }

        if self.selected_square == Some(pos) {
            self.selected_square = None;
            return;
        }

        self.selected_square = Some(pos);
    }

    pub fn deselect_square(&mut self) {
        self.selected_square = None;
    }
}

impl Draw for GameRenderer<'_> {
    fn draw(&self, canvas: &mut sdl2::render::Canvas<sdl2::video::Window>) -> Result<(), String> {
        let flavor = Flavour::Mocha;

        let mut light_squares = [Rect::new(0, 0, 1, 1); 32];
        let mut dark_squares = [Rect::new(0, 0, 1, 1); 32];

        // background
        for i in 0..8 {
            for j in 0..8 {
                let r = Position::new(i, j).to_rect();
                let index = (8 * i as usize + j as usize) / 2;
                if (i + j) % 2 == 0 {
                    light_squares[index] = r;
                } else {
                    dark_squares[index] = r;
                }
            }
        }

        canvas.set_draw_color(flavor.overlay0().as_sdl());
        canvas.fill_rects(&light_squares)?;
        canvas.set_draw_color(flavor.overlay2().as_sdl());
        canvas.fill_rects(&dark_squares)?;

        if let Some(selected) = self.selected_square {
            let color = match self.game.board.get(selected) {
                Some(Some(x)) if x.color == self.game.to_move => flavor.red(),
                _ => flavor.teal(),
            };
            canvas.set_draw_color(color.as_sdl());
            canvas.fill_rect(Some(selected.to_rect()))?;

            for pos in self.game.possible_moves(selected).iter_mut().flatten() {
                canvas.set_draw_color(flavor.peach().as_sdl());
                canvas.fill_rect(Some(pos.to_rect()))?;
            }
        }

        for (pos, piece) in (0..8)
            .cartesian_product(0..8)
            .map(|(x, y)| Position::new(x, y))
            .filter_map(|pos| self.game.board[pos].map(|p| (pos, p)))
        {
            let texture = self.store.piece_texture(piece);
            let rect = pos.to_rect();
            canvas.copy(texture, None, Some(rect))?;
        }

        Ok(())
    }
}
