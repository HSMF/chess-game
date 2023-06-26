use std::{cell::Cell, thread, time::Instant};

use catppuccin::Flavour;
use chess_engine::{BestMove, Cont, Eval};
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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Bot {
    Neither,
    Both,
    White,
    Black,
}

impl Bot {
    pub fn is_bot(&self, player: Player) -> bool {
        match (self, player) {
            (Bot::Neither, _) => false,
            (Bot::Both, _) => true,
            (Bot::White, Player::Black) => false,
            (Bot::White, Player::White) => true,
            (Bot::Black, Player::Black) => true,
            (Bot::Black, Player::White) => false,
        }
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
    bot: Bot,
    game_over: bool,
    selected_square: Option<Position>,
    just_played: Option<Position>,
    did_change: Cell<bool>,

    worker: (
        std::sync::mpsc::Sender<Game>,
        std::sync::mpsc::Receiver<(Eval, Option<Ply>)>,
    ),

    waiting_for_worker: bool,
}

impl<'a> GameRenderer<'a> {
    /// creates a renderer
    pub fn new(texture_creator: &'a TextureCreator<WindowContext>) -> Result<Self, String> {
        let store = TextureStore::new(texture_creator)?;

        let (game_in, game_out) = std::sync::mpsc::channel();
        let (move_in, move_out) = std::sync::mpsc::channel();
        let _ = thread::spawn(move || {
            for game in game_out {
                let start = Instant::now();
                let mut best = BestMove::default();
                const DEPTH: usize = 5;
                let (eval, best_move) =
                    best.best::<tinyvec::ArrayVec<[_; DEPTH + 1]>>(&game, DEPTH);
                eprintln!(
                    "my evaluation is : {eval}: {}.\n'{}'",
                    best_move.iterate().format(", "),
                    game
                );
                eprintln!("took: {}", start.elapsed().as_millis() as f64 / 1000.0);

                move_in
                    .send((eval, best_move.iterate().next().copied()))
                    .unwrap();
            }
        });

        Ok(GameRenderer {
            store,
            bot: Bot::Black,
            game: Game::new(),
            game_over: false,
            selected_square: None,
            just_played: None,
            did_change: Cell::new(true),
            worker: (game_in, move_out),
            waiting_for_worker: false,
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
                Some(piece)
                    if piece.player() == self.game.player_to_move()
                        && !self.bot.is_bot(self.game.player_to_move()) =>
                {
                    if self.game_over {
                        return;
                    }
                    let ply = Ply::from_positions(prev, pos, None, &self.game).unwrap();
                    match self.game.try_make_move(ply) {
                        Ok(..) => {
                            self.just_played = Some(ply.to(self.game.player_to_move().other()));
                        }
                        Err(e) => {
                            eprintln!("error while making a move: {e}")
                        }
                    }

                    self.deselect_square();
                    return;
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

    pub fn check_outcome(&mut self) -> bool {
        if self.game_over {
            return true;
        }
        match self.game.check_outcome() {
            chess_game::game::MoveOutcome::None => {
                return false;
            }
            chess_game::game::MoveOutcome::CanClaimDraw => {
                eprintln!("draw!")
            }
            chess_game::game::MoveOutcome::Checkmate(Player::White) => {
                eprintln!("black won")
            }
            chess_game::game::MoveOutcome::Checkmate(Player::Black) => {
                eprintln!("white won")
            }
        };

        self.game_over = true;
        true
    }

    pub(crate) fn make_bot_move(&mut self) {
        if self.game_over {
            return;
        }
        if self.bot.is_bot(self.game.player_to_move()) {
            if !self.waiting_for_worker {
                self.worker.0.send(self.game.clone()).unwrap();
                self.waiting_for_worker = true;
            } else if let Ok((_, ply)) = self.worker.1.try_recv() {
                if let Some(ply) = ply {
                    self.just_played = Some(ply.to(self.game.player_to_move()));
                    self.game.try_make_move(ply).expect("exists");
                }
                self.check_outcome();
                self.did_change.set(true);
                self.waiting_for_worker = false;
            }
        }
    }

    pub fn set_position(&mut self, pos: &str) -> anyhow::Result<()> {
        self.game = pos.parse()?;
        Ok(())
    }

    pub fn set_bot(&mut self, bot: Bot) {
        eprintln!("{bot:?}");
        self.bot = bot;
    }
}

impl Draw for GameRenderer<'_> {
    fn draw(&self, canvas: &mut sdl2::render::Canvas<sdl2::video::Window>) -> Result<bool, String> {
        let changed = self.did_change.get();
        if !changed {
            return Ok(false);
        }

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

        if let Some(just_played) = self.just_played {
            canvas.set_draw_color(flavor.pink().as_sdl());
            canvas.fill_rect(Some(just_played.to_rect()))?;
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

        self.did_change.set(false);

        Ok(true)
    }
}
