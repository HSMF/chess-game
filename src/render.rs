use std::cell::Cell;

use catppuccin::Flavour;
use chess_game::{Game, PieceKind, Player, Position};
use itertools::Itertools;
use sdl2::{
    image::LoadTexture,
    rect::Rect,
    render::{Texture, TextureCreator},
    video::WindowContext,
};

use chess_game::{Piece, Ply};

use crate::graphics::{AsRgba, Draw};

trait PositionExt {
    fn from_physical(x: u32, y: u32) -> Self;
    fn to_rect(self) -> Rect;
}

impl PositionExt for Position {
    fn from_physical(x: u32, y: u32) -> Self {
        use crate::graphics::WIDTH;
        let x = (x / WIDTH) as u8;
        let y = ((8 * WIDTH - y) / WIDTH) as u8;
        Self::new(x, y)
    }

    fn to_rect(self) -> Rect {
        use crate::graphics::WIDTH;

        Rect::new(
            self.x() as i32 * WIDTH as i32,
            7 * WIDTH as i32 - self.y() as i32 * WIDTH as i32,
            WIDTH,
            WIDTH,
        )
    }
}

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
        match (piece.player(), piece.kind()) {
            (Player::Black, PieceKind::Pawn) => &self.black_pawn,
            (Player::Black, PieceKind::Rook) => &self.black_rook,
            (Player::Black, PieceKind::Knight) => &self.black_knight,
            (Player::Black, PieceKind::Bishop) => &self.black_bishop,
            (Player::Black, PieceKind::Queen) => &self.black_queen,
            (Player::Black, PieceKind::King) => &self.black_king,
            (Player::White, PieceKind::Pawn) => &self.white_pawn,
            (Player::White, PieceKind::Rook) => &self.white_rook,
            (Player::White, PieceKind::Knight) => &self.white_knight,
            (Player::White, PieceKind::Bishop) => &self.white_bishop,
            (Player::White, PieceKind::Queen) => &self.white_queen,
            (Player::White, PieceKind::King) => &self.white_king,
        }
    }
}

/// Wrapper that renders a [`Game`]
pub struct GameRenderer<'a> {
    store: TextureStore<'a>,
    game: Game,
    selected_square: Option<Position>,
    did_change: Cell<bool>,
}

impl<'a> GameRenderer<'a> {
    /// creates a renderer
    pub fn new(texture_creator: &'a TextureCreator<WindowContext>) -> Result<Self, String> {
        let store = TextureStore::new(texture_creator)?;
        Ok(GameRenderer {
            store,
            game: Game::new(),
            selected_square: None,
            did_change: Cell::new(true),
        })
    }

    /// does the selection action for the square at the coordinates.
    ///
    /// - if nothing was selected, select it
    /// - if something was already selected, try to move from the selected square to the target
    ///   square
    /// - otherwise, select the new square
    pub fn select_square(&mut self, x: u32, y: u32) {
        let pos = Position::from_physical(x, y);
        self.did_change.set(true);

        if let Some(prev) = self.selected_square {
            if prev == pos {
                self.selected_square = None;
                return;
            }
            match self.game[prev] {
                Some(piece) if piece.player() == self.game.player_to_move() => {
                    let ply = if piece.is_king() && prev.distance(pos) > 1 {
                        if pos.x() == 2 {
                            Ply::LongCastle
                        } else {
                            Ply::Castle
                        }
                    } else {
                        Ply::Move {
                            from: prev,
                            to: pos,
                            promoted_to: None,
                        }
                    };
                    match self.game.try_make_move(ply) {
                        Ok(()) => {}
                        Err(e) => {
                            eprintln!("error while making a move: {e}")
                        }
                    }

                    match self.game.check_outcome() {
                        chess_game::game::MoveOutcome::None => {}
                        chess_game::game::MoveOutcome::CanClaimDraw => {
                            eprintln!("draw!")
                        }
                        chess_game::game::MoveOutcome::Checkmate(Player::White) => {
                            eprintln!("black won")
                        }
                        chess_game::game::MoveOutcome::Checkmate(Player::Black) => {
                            eprintln!("white won")
                        }
                    }
                }
                _ => {}
            }
        }

        if self.selected_square == Some(pos) {
            self.selected_square = None;
            return;
        }

        self.selected_square = Some(pos);
    }

    /// removes focus from the focused square
    pub fn deselect_square(&mut self) {
        self.did_change.set(true);
        self.selected_square = None;
    }
}

impl Draw for GameRenderer<'_> {
    fn draw(&self, canvas: &mut sdl2::render::Canvas<sdl2::video::Window>) -> Result<bool, String> {
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
            let color = match self.game[selected] {
                Some(x) if x.player() == self.game.player_to_move() => flavor.red(),
                _ => flavor.teal(),
            };
            canvas.set_draw_color(color.as_sdl());
            canvas.fill_rect(Some(selected.to_rect()))?;

            for pos in self.game.possible_moves(selected).iter_mut().flatten() {
                canvas.set_draw_color(flavor.peach().as_sdl());
                canvas.fill_rect(Some(pos.to_rect()))?;
            }
        }

        if self.game.is_in_check(self.game.player_to_move()) {
            let king = self
                .game
                .pieces()
                .find(|(_, piece)| piece.is_king() && piece.player() == self.game.player_to_move())
                .map(|x| x.0);

            if let Some(king) = king {
                canvas.set_draw_color(flavor.red().as_sdl());
                canvas.fill_rect(Some(king.to_rect()))?;
            }
        }

        for (pos, piece) in (0..8)
            .cartesian_product(0..8)
            .map(|(x, y)| Position::new(x, y))
            .filter_map(|pos| self.game[pos].map(|p| (pos, p)))
        {
            let texture = self.store.piece_texture(piece);
            let rect = pos.to_rect();
            canvas.copy(texture, None, Some(rect))?;
        }

        let changed = self.did_change.get();
        self.did_change.set(false);

        Ok(changed)
    }
}
