use std::ops::Index;
use std::{fmt::Display, str::FromStr};

use crate::ply::Ply;
use crate::{Piece, PieceKind, Player};
mod blockable_pieces;
mod board;
mod render;

use crate::Position;
pub use blockable_pieces::{BishopMove, KnightMove, Mover, PawnMove, QueenMove, RookMove, KingMove};
pub use board::Board;
use either::Either;
use itertools::Itertools;
use nom::{
    branch::alt,
    character::complete::{char as nchar, digit1, one_of},
    combinator::{fail, opt},
    multi::many_m_n,
    sequence::tuple,
    IResult, Parser,
};
pub use render::GameRenderer;

use self::blockable_pieces::PieceMove;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct CastlingRights {
    queen_side: bool,
    king_side: bool,
}

impl CastlingRights {
    pub const fn none() -> Self {
        Self {
            queen_side: false,
            king_side: false,
        }
    }

    pub const fn all() -> Self {
        Self {
            queen_side: true,
            king_side: true,
        }
    }
}

impl Default for CastlingRights {
    fn default() -> Self {
        Self::all()
    }
}

impl Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.board)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Game {
    pub board: Board,
    /// Some(pos) if a pawn has been double-pushed to pos on the turn before that
    pub(crate) en_passant_sq: Option<Position>,
    castling_white: CastlingRights,
    castling_black: CastlingRights,
    /// Which player color is next to move
    to_move: Player,
    /// how many turns since last
    /// - capture
    /// - pawn move
    /// - loss of castling rights
    halfmove_clock: usize,
    /// number of full moves
    fullmove_clock: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum FenError {
    #[error("trailing characters")]
    TrailingChars,

    // TODO: add details
    #[error("failed to parse (sorry, no details yet)")]
    ParseError,
}

#[derive(Debug, Clone)]
pub struct PiecesIter<'a> {
    idx: u8,
    game: &'a Game,
}

impl<'a> Iterator for PiecesIter<'a> {
    type Item = (Position, Piece);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let x = self.idx % 8;
            let y = self.idx / 8;
            let pos = Position::try_new(x, y)?;
            self.idx += 1;
            if let Some(piece) = self.game[pos] {
                return Some((pos, piece));
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{ply}")]
pub struct MoveError {
    ply: Ply,
}

#[derive(Debug, PartialEq, Eq)]
enum Action {
    Move(Position, Position),
    Kill(Position),
}
impl Default for Action {
    fn default() -> Self {
        Action::Kill(Position::default())
    }
}

impl Game {
    pub fn new() -> Self {
        Game {
            board: Board::new(),
            en_passant_sq: None,
            castling_white: CastlingRights::default(),
            castling_black: CastlingRights::default(),
            to_move: Player::White,
            halfmove_clock: 0,
            fullmove_clock: 1,
        }
    }

    pub fn to_move(&self) -> Player {
        self.to_move
    }

    /// Attempts to make a move, returning Err if the given move was not valid
    pub fn try_make_move(&mut self, ply: Ply) -> anyhow::Result<()> {
        let mut reset_counter = false;
        let mut castling_rights = *self.castling_rights(self.to_move);
        let mut en_passant_sq = None;

        let moves = match ply {
            Ply::Move {
                from,
                to,
                promoted_to: _,
            } => {
                let mut possible_moves = self
                    .possible_moves(from)
                    .filter(|p| p.player() == self.to_move)
                    .ok_or(anyhow::anyhow!("none of your pieces is on {from}"))?;

                if possible_moves.is_pawn() {
                    reset_counter = true;
                }

                if !possible_moves.contains(&to) {
                    anyhow::bail!("cannot move to {to}.")
                }

                if possible_moves.is_pawn() && from.distance(to) > 1 {
                    en_passant_sq = Some(match self.to_move {
                        Player::Black => (from + (0, -1)).unwrap(),
                        Player::White => (from + (0, 1)).unwrap(),
                    });
                }

                let mut vec = tinyvec::array_vec!([Action; 2] => Action::Move(from, to));
                if possible_moves.is_pawn() && Some(to) == self.en_passant_sq {
                    let pos = match self.to_move.other() {
                        Player::Black => (to + (0, -1)).unwrap(),
                        Player::White => (to + (0, 1)).unwrap(),
                    };
                    vec.push(Action::Kill(pos));
                }

                vec
            }
            Ply::Castle => {
                anyhow::ensure!(
                    self.castling_rights(self.to_move).king_side,
                    "cannot castle on king's side anymore"
                );
                let home_row = match self.to_move {
                    Player::Black => 7,
                    Player::White => 0,
                };
                let king_from = Position::new(4, home_row);
                let king_to = Position::new(6, home_row);

                let mut possible_moves = self
                    .possible_moves(king_from)
                    .filter(|p| p.player() == self.to_move)
                    .filter(|p| p.is_king())
                    .ok_or(anyhow::anyhow!("king isn't on {king_from}"))?;

                anyhow::ensure!(
                    possible_moves.contains(&king_to),
                    "cannot castle to {king_to}"
                );

                let rook_from = Position::new(7, home_row);
                let rook_to = Position::new(5, home_row);
                castling_rights = CastlingRights::none();

                tinyvec::array_vec!([Action; 2] => Action::Move(king_from, king_to), Action::Move(rook_from, rook_to))
            }
            Ply::LongCastle => {
                anyhow::ensure!(
                    self.castling_rights(self.to_move).queen_side,
                    "cannot castle on queen's side anymore"
                );
                let home_row = match self.to_move {
                    Player::Black => 7,
                    Player::White => 0,
                };
                let king_from = Position::new(4, home_row);
                let king_to = Position::new(2, home_row);

                let mut possible_moves = self
                    .possible_moves(king_from)
                    .filter(|p| p.player() == self.to_move)
                    .filter(|p| p.is_king())
                    .ok_or(anyhow::anyhow!("king isn't on {king_from}"))?;

                anyhow::ensure!(
                    possible_moves.contains(&king_to),
                    "cannot castle to {king_to}"
                );

                let rook_from = Position::new(0, home_row);
                let rook_to = Position::new(3, home_row);
                castling_rights = CastlingRights::none();

                tinyvec::array_vec!([Action; 2] => Action::Move(king_from, king_to), Action::Move(rook_from, rook_to))
            }
        };

        let mut old_state = tinyvec::array_vec!([(Position, Piece); 4]);
        for action in moves {
            match action {
                Action::Kill(pos) => {
                    if let Some(piece) = self.board[pos] {
                        old_state.push((pos, piece));
                    }
                    eprintln!("google en passant (holy hell)");
                    self.board[pos] = None;
                }
                Action::Move(from, to) => {
                    // make a backup
                    let moving_piece = self.board[from].expect("must have a piece here");
                    old_state.push((from, moving_piece));
                    if let Some(piece) = self.board[to] {
                        old_state.push((to, piece));
                        reset_counter = true;
                    }
                    self.board[to] = Some(moving_piece);
                    self.board[from] = None;
                }
            }
        }

        // check if the turn was valid, else revert

        if reset_counter {
            self.halfmove_clock = 0;
        } else {
            self.halfmove_clock += 1;
        }
        if self.to_move == Player::Black {
            self.fullmove_clock += 1;
        }

        *self.castling_rights_mut(self.to_move) = castling_rights;
        self.en_passant_sq = en_passant_sq;

        self.to_move.flip();
        Ok(())
    }

    pub fn possible_moves(&self, piece_pos: Position) -> Option<PieceMove> {
        self.board[piece_pos]?;
        Some(PieceMove::new(piece_pos, self))
    }

    /// Get the castling rights for the given player
    ///
    /// ## Examples
    /// ```
    /// # use chess_game::{Game, Player, game::CastlingRights};
    /// let game: Game = "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    ///                 .parse()
    ///                 .unwrap();
    /// assert_eq!(game.castling_rights(Player::Black), &CastlingRights::all());
    /// ```
    pub fn castling_rights(&self, player: Player) -> &CastlingRights {
        match player {
            Player::Black => &self.castling_black,
            Player::White => &self.castling_white,
        }
    }

    pub fn castling_rights_mut(&mut self, player: Player) -> &mut CastlingRights {
        match player {
            Player::Black => &mut self.castling_black,
            Player::White => &mut self.castling_white,
        }
    }

    /// Returns an iterator over the pieces in the board.
    ///
    /// This gives no guarantees over the order
    ///
    /// ## Example
    /// ```
    /// # use chess_game::game::Game;
    /// let game = Game::new();
    /// for (position, piece) in game.pieces() {
    ///     println!("there is the piece {piece} at {position}");
    /// }
    /// ```
    pub fn pieces(&self) -> PiecesIter {
        PiecesIter { idx: 0, game: self }
    }

    #[cfg(test)]
    pub(crate) fn to_move_mut(&mut self) -> &mut Player {
        &mut self.to_move
    }

    /// Parses a [FEN](https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation) string.
    ///
    /// ### Examples
    /// ```
    /// # use chess_game::{Game, Player, game::CastlingRights};
    /// let game: Game = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    ///                 .parse()
    ///                 .unwrap();
    /// assert_eq!(game, Game::new());
    /// ```
    pub fn from_fen(s: &str) -> Result<Self, FenError> {
        let (
            s,
            (
                board,
                _,
                to_move,
                _,
                (castling_white, castling_black),
                _,
                en_passant_sq,
                _,
                halfmove_clock,
                _,
                fullmove_clock,
            ),
        ) = tuple((
            piece_placement,
            nchar(' '),
            side_to_move,
            nchar(' '),
            castling_ability,
            nchar(' '),
            en_passant_sq,
            nchar(' '),
            counter,
            nchar(' '),
            counter,
        ))(s)
        .map_err(|_| FenError::ParseError)?;

        if !s.is_empty() {
            return Err(FenError::TrailingChars);
        }

        Ok(Game {
            board,
            en_passant_sq,
            castling_white,
            castling_black,
            to_move,
            halfmove_clock,
            fullmove_clock,
        })
    }
}

impl Index<Position> for Game {
    type Output = Option<Piece>;

    fn index(&self, index: Position) -> &Self::Output {
        &self.board[index]
    }
}

impl Default for Game {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Field {
    /// a piece
    Piece(Piece),
    /// `self.0` empty fields
    Empty(u8),
}

impl Default for Field {
    fn default() -> Self {
        Self::Empty(1)
    }
}

fn field(s: &str) -> IResult<&str, Field> {
    let (s, x) = one_of("rnbqkpRNBQKP12345678")(s)?;
    let field = match x {
        'r' => Field::Piece(Piece::new_black(PieceKind::Rook)),
        'n' => Field::Piece(Piece::new_black(PieceKind::Knight)),
        'b' => Field::Piece(Piece::new_black(PieceKind::Bishop)),
        'q' => Field::Piece(Piece::new_black(PieceKind::Queen)),
        'k' => Field::Piece(Piece::new_black(PieceKind::King)),
        'p' => Field::Piece(Piece::new_black(PieceKind::Pawn)),
        'R' => Field::Piece(Piece::new_white(PieceKind::Rook)),
        'N' => Field::Piece(Piece::new_white(PieceKind::Knight)),
        'B' => Field::Piece(Piece::new_white(PieceKind::Bishop)),
        'Q' => Field::Piece(Piece::new_white(PieceKind::Queen)),
        'K' => Field::Piece(Piece::new_white(PieceKind::King)),
        'P' => Field::Piece(Piece::new_white(PieceKind::Pawn)),
        x @ ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8') => Field::Empty(x as u8 - b'0'),
        _ => unreachable!(),
    };
    Ok((s, field))
}

fn side_to_move(s: &str) -> IResult<&str, Player> {
    let (s, player) = one_of("wb")(s)?;
    let player = if player == 'w' {
        Player::White
    } else {
        Player::Black
    };
    Ok((s, player))
}

fn rank(s: &str) -> IResult<&str, Vec<Field>> {
    // TODO: consider tinyvec
    let (s, fields) = many_m_n(1, 8, field)(s)?;
    Ok((s, fields))
}

fn piece_placement(s: &str) -> IResult<&str, Board> {
    let (s, before) = many_m_n(7, 7, tuple((rank, nchar('/'))))(s)?;
    let (s, last) = rank(s)?;

    let mut fields = [None; 64];
    let mut i = 0;
    for val in before
        .into_iter()
        .flat_map(|(x, _)| x)
        .chain(last.into_iter())
    {
        match val {
            Field::Piece(piece) => {
                fields[i] = Some(piece);
                i += 1;
            }
            Field::Empty(n) => i += n as usize,
        }
    }

    Ok((s, Board { fields }))
}

fn castling_ability(s: &str) -> IResult<&str, (CastlingRights, CastlingRights)> {
    let (s, castling) = alt((
        nchar('-').map(Either::Left),
        tuple((
            opt(nchar('K')),
            opt(nchar('Q')),
            opt(nchar('k')),
            opt(nchar('q')),
        ))
        .map(Either::Right),
    ))(s)?;

    let (castle_white, castle_black) = match castling {
        Either::Left(_) => (CastlingRights::none(), CastlingRights::none()),
        Either::Right((wk, wq, bk, bq)) => (
            CastlingRights {
                king_side: wk.is_some(),
                queen_side: wq.is_some(),
            },
            CastlingRights {
                king_side: bk.is_some(),
                queen_side: bq.is_some(),
            },
        ),
    };

    Ok((s, (castle_white, castle_black)))
}

fn en_passant_sq(s: &str) -> IResult<&str, Option<Position>> {
    let (s, sq) = alt((
        nchar('-').map(Either::Left),
        tuple((one_of("abcdefgh"), one_of("36"))).map(Either::Right),
    ))(s)?;

    let sq = sq
        .right()
        .map(|(x, y)| Position::new(x as u8 - b'a', y as u8 - b'1'));

    Ok((s, sq))
}

fn counter(s: &str) -> IResult<&str, usize> {
    let (s, num) = digit1(s)?;
    let Ok(num) = num.parse() else {
        fail::<_, (), _>(s)?;
        unreachable!();
    };

    Ok((s, num))
}

impl FromStr for Game {
    type Err = FenError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_fen(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pieces::*;
    use pretty_assertions::assert_eq;

    mod pieces {
        #![allow(non_upper_case_globals, unused)]
        use crate::game::{Piece, PieceKind};
        pub const R: Option<Piece> = Some(Piece::new_white(PieceKind::Rook));
        pub const N: Option<Piece> = Some(Piece::new_white(PieceKind::Knight));
        pub const B: Option<Piece> = Some(Piece::new_white(PieceKind::Bishop));
        pub const Q: Option<Piece> = Some(Piece::new_white(PieceKind::Queen));
        pub const K: Option<Piece> = Some(Piece::new_white(PieceKind::King));
        pub const P: Option<Piece> = Some(Piece::new_white(PieceKind::Pawn));

        pub const r: Option<Piece> = Some(Piece::new_black(PieceKind::Rook));
        pub const n: Option<Piece> = Some(Piece::new_black(PieceKind::Knight));
        pub const b: Option<Piece> = Some(Piece::new_black(PieceKind::Bishop));
        pub const q: Option<Piece> = Some(Piece::new_black(PieceKind::Queen));
        pub const k: Option<Piece> = Some(Piece::new_black(PieceKind::King));
        pub const p: Option<Piece> = Some(Piece::new_black(PieceKind::Pawn));

        pub const x: Option<Piece> = None;
    }

    #[test]
    fn starting_position() {
        let fen: Game = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            .parse()
            .unwrap();
        let expected = Game::new();
        assert_eq!(fen, expected);
    }

    #[test]
    fn lichess_() {
        let from_fen: Game = "2k5/2p2pb1/3p2pp/3P4/p1PP1B2/N4P2/1r3KPP/8 w - - 2 30"
            .parse()
            .unwrap();
        let expected = Game {
            #[rustfmt::skip]
            board: Board { fields: [
                x,x,k,x,x,x,x,x,
                x,x,p,x,x,p,b,x,
                x,x,x,p,x,x,p,p,
                x,x,x,P,x,x,x,x,
                p,x,P,P,x,B,x,x,
                N,x,x,x,x,P,x,x,
                x,r,x,x,x,K,P,P,
                x,x,x,x,x,x,x,x,
                ] },
            en_passant_sq: None,
            castling_white: CastlingRights::none(),
            castling_black: CastlingRights::none(),
            to_move: Player::White,
            halfmove_clock: 2,
            fullmove_clock: (30),
        };
        assert_eq!(from_fen, expected);
    }

    fn relaxed_game_eq(got: &Game, expected: &Game) {
        assert_eq!(got.board, expected.board);
        if expected.en_passant_sq.is_some() {
            assert_eq!(got.en_passant_sq, expected.en_passant_sq);
        }
        assert_eq!(got.castling_white, expected.castling_white);
        assert_eq!(got.castling_black, expected.castling_black);
        assert_eq!(got.to_move, expected.to_move);
        assert_eq!(got.halfmove_clock, expected.halfmove_clock);
        assert_eq!(got.fullmove_clock, expected.fullmove_clock);
    }

    fn test_game(
        positions: impl IntoIterator<Item = &'static str>,
        san_moves: impl IntoIterator<Item = &'static str>,
    ) {
        let positions = positions.into_iter();
        let san_moves = san_moves.into_iter();
        let mut game = Game::new();
        for (pos, ply) in positions.zip(san_moves) {
            match game.to_move {
                Player::Black => eprint!("    "),
                Player::White => eprint!("{:2}. ", game.fullmove_clock),
            }
            eprintln!("{:?} {ply}", game.to_move);
            let pos: Game = pos.parse().unwrap();
            let ply = Ply::parse_san(ply, &game).unwrap();
            game.try_make_move(ply).unwrap();
            relaxed_game_eq(&game, &pos)
        }
    }

    macro_rules! make_move {
        ($name:ident, $the_move:literal :: $before_fen:literal -> $after_fen:literal) => {
            #[test]
            fn $name() {
                let mut before: Game = $before_fen.parse().expect("should be a valid fen string");
                let after: Game = $after_fen.parse().expect("should be a valid fen string");
                let the_move = Ply::parse_pure($the_move).expect("should be a valid pure move");
                before.try_make_move(the_move).expect("was a valid move");

                relaxed_game_eq(&before, &after);
            }
        };
    }

    make_move!(first, "e2e4" :: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                             -> "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1");
    make_move!(second, "d7d5" :: "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
                             ->  "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2");

    #[test]
    #[allow(non_snake_case)]
    fn lichess_3Pon25ma() {
        let positions = [
            "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1",
            "rnbqkb1r/pppppppp/5n2/8/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 1 2",
            "rnbqkb1r/pppppppp/5n2/8/2PP4/8/PP2PPPP/RNBQKBNR b KQkq - 0 2",
            "rnbqkb1r/pppp1ppp/4pn2/8/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 3",
            "rnbqkb1r/pppp1ppp/4pn2/6B1/2PP4/8/PP2PPPP/RN1QKBNR b KQkq - 1 3",
            "rnbqk2r/pppp1ppp/4pn2/6B1/1bPP4/8/PP2PPPP/RN1QKBNR w KQkq - 2 4",
            "rnbqk2r/pppp1ppp/4pn2/6B1/1bPP4/8/PP1NPPPP/R2QKBNR b KQkq - 3 4",
            "rnbqk2r/pppp1pp1/4pn1p/6B1/1bPP4/8/PP1NPPPP/R2QKBNR w KQkq - 0 5",
            "rnbqk2r/pppp1pp1/4pn1p/8/1bPP3B/8/PP1NPPPP/R2QKBNR b KQkq - 1 5",
            "rnbqk2r/pppp1p2/4pn1p/6p1/1bPP3B/8/PP1NPPPP/R2QKBNR w KQkq - 0 6",
            "rnbqk2r/pppp1p2/4pn1p/6p1/1bPP4/6B1/PP1NPPPP/R2QKBNR b KQkq - 1 6",
            "rnbqk2r/pppp1p2/4p2p/6p1/1bPPn3/6B1/PP1NPPPP/R2QKBNR w KQkq - 2 7",
            "rnbqk2r/pppp1p2/4p2p/6p1/1bPPn3/5NB1/PP1NPPPP/R2QKB1R b KQkq - 3 7",
            "rnbqk2r/pppp4/4p2p/5pp1/1bPPn3/5NB1/PP1NPPPP/R2QKB1R w KQkq - 0 8",
            "rnbqk2r/pppp4/4p2p/5pp1/1bPPn3/4PNB1/PP1N1PPP/R2QKB1R b KQkq - 0 8",
            "rnbqk2r/pppp4/4p3/5ppp/1bPPn3/4PNB1/PP1N1PPP/R2QKB1R w KQkq - 0 9",
            "rnbqk2r/pppp4/4p3/5ppp/1bPPn3/P3PNB1/1P1N1PPP/R2QKB1R b KQkq - 0 9",
            "rnbqk2r/pppp4/4p3/5ppp/2PPn3/P3PNB1/1P1b1PPP/R2QKB1R w KQkq - 0 10",
            "rnbqk2r/pppp4/4p3/5ppp/2PPn3/P3P1B1/1P1N1PPP/R2QKB1R b KQkq - 0 10",
            "rnbqk2r/pppp4/4p3/5ppp/2PP4/P3P1B1/1P1n1PPP/R2QKB1R w KQkq - 0 11",
            "rnbqk2r/pppp4/4p3/5ppp/2PP4/P3P1B1/1P1Q1PPP/R3KB1R b KQkq - 0 11",
            "rnbqk2r/ppp5/3pp3/5ppp/2PP4/P3P1B1/1P1Q1PPP/R3KB1R w KQkq - 0 12",
            "rnbqk2r/ppp5/3pp3/5ppp/2PP3P/P3P1B1/1P1Q1PP1/R3KB1R b KQkq - 0 12",
            "rnbqk2r/ppp5/3pp3/5p1p/2PP2pP/P3P1B1/1P1Q1PP1/R3KB1R w KQkq - 0 13",
            "rnbqk2r/ppp5/3pp3/5p1p/2PP2pP/P2BP1B1/1P1Q1PP1/R3K2R b KQkq - 1 13",
            "rnbqk2r/p1p5/1p1pp3/5p1p/2PP2pP/P2BP1B1/1P1Q1PP1/R3K2R w KQkq - 0 14",
            "rnbqk2r/p1p5/1p1pp3/5p1p/2PP2pP/P2BP1B1/1P1Q1PP1/2KR3R b kq - 1 14",
            "rn1qk2r/pbp5/1p1pp3/5p1p/2PP2pP/P2BP1B1/1P1Q1PP1/2KR3R w kq - 2 15",
            "rn1qk2r/pbp5/1p1pp3/3P1p1p/2P3pP/P2BP1B1/1P1Q1PP1/2KR3R b kq - 0 15",
            "rn2k2r/pbp5/1p1ppq2/3P1p1p/2P3pP/P2BP1B1/1P1Q1PP1/2KR3R w kq - 1 16",
            "rn2k2r/pbp5/1p1ppq2/3P1p1p/2P1P1pP/P2B2B1/1P1Q1PP1/2KR3R b kq - 0 16",
            "r3k2r/pbpn4/1p1ppq2/3P1p1p/2P1P1pP/P2B2B1/1P1Q1PP1/2KR3R w kq - 1 17",
            "r3k2r/pbpn4/1p1ppq2/3P1P1p/2P3pP/P2B2B1/1P1Q1PP1/2KR3R b kq - 0 17",
            "r3k2r/pbp5/1p1ppq2/2nP1P1p/2P3pP/P2B2B1/1P1Q1PP1/2KR3R w kq - 1 18",
            "r3k2r/pbp5/1p1pPq2/2nP3p/2P3pP/P2B2B1/1P1Q1PP1/2KR3R b kq - 0 18",
            "r3k2r/pbp5/1p1pPq2/3P3p/2P3pP/Pn1B2B1/1P1Q1PP1/2KR3R w kq - 1 19",
            "r3k2r/pbp5/1p1pPq2/3P3p/2P3pP/Pn1B2B1/1PKQ1PP1/3R3R b kq - 2 19",
            "r3k2r/pbp5/1p1pPq2/3P3p/2P3pP/P2B2B1/1PKn1PP1/3R3R w kq - 0 20",
            "r3k2r/pbp5/1p1pPq2/3P3p/2P3pP/P2B2B1/1PKR1PP1/7R b kq - 0 20",
            "2kr3r/pbp5/1p1pPq2/3P3p/2P3pP/P2B2B1/1PKR1PP1/7R w - - 1 21",
            "2kr3r/pbp5/1p1pPq2/3P3p/2P3pP/P2B2B1/1PKR1PP1/4R3 b - - 2 21",
            "2k1r2r/pbp5/1p1pPq2/3P3p/2P3pP/P2B2B1/1PKR1PP1/4R3 w - - 3 22",
            "2k1r2r/pbp5/1p1pPq2/3P3p/2P3pP/P2B2B1/1PK1RPP1/4R3 b - - 4 22",
            "1k2r2r/pbp5/1p1pPq2/3P3p/2P3pP/P2B2B1/1PK1RPP1/4R3 w - - 5 23",
            "1k2r2r/pbp5/1p1pPq2/3P3p/2P3pP/PP1B2B1/2K1RPP1/4R3 b - - 0 23",
            "1kb1r2r/p1p5/1p1pPq2/3P3p/2P3pP/PP1B2B1/2K1RPP1/4R3 w - - 1 24",
            "1kb1r2r/p1p5/1p1pPq2/3P3p/2P3pP/PP1BR1B1/2K2PP1/4R3 b - - 2 24",
            "1kb4r/p1p1r3/1p1pPq2/3P3p/2P3pP/PP1BR1B1/2K2PP1/4R3 w - - 3 25",
            "1kb4r/p1p1r3/1p1pPq2/3P3p/2P2PpP/PP1BR1B1/2K3P1/4R3 b - f3 0 25",
            "1kb4r/p1p1r3/1p1pPq2/3P3p/2P4P/PP1BRpB1/2K3P1/4R3 w - - 0 26",
            "1kb4r/p1p1r3/1p1pPq2/3P3p/2P4P/PP1B1RB1/2K3P1/4R3 b - - 0 26",
            "1kb4r/p1p1r1q1/1p1pP3/3P3p/2P4P/PP1B1RB1/2K3P1/4R3 w - - 1 27",
            "1kb4r/p1p1r1q1/1p1pP3/3P1B1p/2P4P/PP3RB1/2K3P1/4R3 b - - 2 27",
            "1kb2r2/p1p1r1q1/1p1pP3/3P1B1p/2P4P/PP3RB1/2K3P1/4R3 w - - 3 28",
            "1kb2r2/p1p1r1q1/1p1pP3/3P1B1p/2P4P/PP3RB1/2K1R1P1/8 b - - 4 28",
            "1kb5/p1p1r1q1/1p1pP3/3P1r1p/2P4P/PP3RB1/2K1R1P1/8 w - - 0 29",
        ];

        let moves = [
            "d4", "Nf6", "c4", "e6", "Bg5", "Bb4", "Nd2", "h6", "Bh4", "g5", "Bg3", "Ne4", "Ng1f3",
            "f5", "e3", "h5", "a3", "Bb4xd2", "Nxd2", "Nxd2", "Qd1xd2", "d6", "h4", "g4", "Bf1d3",
            "b6", "O-O-O", "Bb7", "d5", "Qf6", "e4", "Nd7", "exf5", "Nc5", "fxe6", "Nb3", "Kc2",
            "Nxd2", "Rxd2", "O-O-O", "Re1", "Rd8e8", "Rd2e2", "Kc8b8", "b3", "Bc8", "Re3", "Re7",
            "f4", "gxf3", "Rxf3", "Qg7", "Bf5", "Rf8", "Re2", "Rxf5",
        ];

        test_game(positions, moves);
    }

    #[test]
    #[allow(non_snake_case)]
    fn lichess_mM3VkF7P() {
        let positions = [
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1",
            "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
            "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
            "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3",
            "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3",
            "r1bqkbnr/1ppp1ppp/p1n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4",
            "r1bqkbnr/1ppp1ppp/p1B5/4p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 0 4",
            "r1bqkbnr/1pp2ppp/p1p5/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 5",
            "r1bqkbnr/1pp2ppp/p1p5/4p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 5",
            "r2qkbnr/1pp2ppp/p1p5/4p3/4P1b1/5N2/PPPP1PPP/RNBQ1RK1 w kq - 2 6",
            "r2qkbnr/1pp2ppp/p1p5/4p3/4P1b1/5N1P/PPPP1PP1/RNBQ1RK1 b kq - 0 6",
            "r2qkbnr/1pp2pp1/p1p5/4p2p/4P1b1/5N1P/PPPP1PP1/RNBQ1RK1 w kq - 0 7",
            "r2qkbnr/1pp2pp1/p1p5/4p2p/4P1b1/3P1N1P/PPP2PP1/RNBQ1RK1 b kq - 0 7",
            "r3kbnr/1pp2pp1/p1p2q2/4p2p/4P1b1/3P1N1P/PPP2PP1/RNBQ1RK1 w kq - 1 8",
            "r3kbnr/1pp2pp1/p1p2q2/4p2p/4P1b1/2NP1N1P/PPP2PP1/R1BQ1RK1 b kq - 2 8",
            "r3k1nr/1pp2pp1/p1pb1q2/4p2p/4P1b1/2NP1N1P/PPP2PP1/R1BQ1RK1 w kq - 3 9",
            "r3k1nr/1pp2pp1/p1pb1q2/4p2p/4P1b1/2NPBN1P/PPP2PP1/R2Q1RK1 b kq - 4 9",
            "r3k2r/1pp1npp1/p1pb1q2/4p2p/4P1b1/2NPBN1P/PPP2PP1/R2Q1RK1 w kq - 5 10",
            "r3k2r/1pp1npp1/p1pb1q2/4p2p/4P1b1/2NPBN1P/PPP1QPP1/R4RK1 b kq - 6 10",
            "r3k2r/1pp2pp1/p1pb1qn1/4p2p/4P1b1/2NPBN1P/PPP1QPP1/R4RK1 w kq - 7 11",
            "r3k2r/1pp2pp1/p1pb1qn1/4p2p/4P1P1/2NPBN2/PPP1QPP1/R4RK1 b kq - 0 11",
            "r3k2r/1pp2pp1/p1pb1qn1/4p3/4P1p1/2NPBN2/PPP1QPP1/R4RK1 w kq - 0 12",
            "r3k2r/1pp2pp1/p1pb1qn1/4p1N1/4P1p1/2NPB3/PPP1QPP1/R4RK1 b kq - 1 12",
            "r3k2r/1pp2pp1/p1pb1q2/4p1N1/4Pnp1/2NPB3/PPP1QPP1/R4RK1 w kq - 2 13",
            "r3k2r/1pp2pp1/p1pb1q2/4p1N1/4PBp1/2NP4/PPP1QPP1/R4RK1 b kq - 0 13",
            "r3k2r/1pp2pp1/p1pb4/4p1N1/4Pqp1/2NP4/PPP1QPP1/R4RK1 w kq - 0 14",
            "r3k2r/1pp2pp1/p1pb4/4p1N1/4Pqp1/2NP2P1/PPP1QP2/R4RK1 b kq - 0 14",
            "r3k2r/1pp2pp1/p1pb4/4p1q1/4P1p1/2NP2P1/PPP1QP2/R4RK1 w kq - 0 15",
            "r3k2r/1pp2pp1/p1pb4/4p1q1/4P1p1/2NP2P1/PPP1QPK1/R4R2 b kq - 1 15",
            "2kr3r/1pp2pp1/p1pb4/4p1q1/4P1p1/2NP2P1/PPP1QPK1/R4R2 w - - 2 16",
            "2kr3r/1pp2pp1/p1pb4/4p1q1/4P1p1/3P2P1/PPP1QPK1/R2N1R2 b - - 3 16",
            "1k1r3r/1pp2pp1/p1pb4/4p1q1/4P1p1/3P2P1/PPP1QPK1/R2N1R2 w - - 4 17",
            "1k1r3r/1pp2pp1/p1pb4/4p1q1/2P1P1p1/3P2P1/PP2QPK1/R2N1R2 b - - 0 17",
            "1k1r3r/1pp2pp1/p1p5/2b1p1q1/2P1P1p1/3P2P1/PP2QPK1/R2N1R2 w - - 1 18",
            "1k1r3r/1pp2pp1/p1p5/2b1p1q1/2P1P1p1/P2P2P1/1P2QPK1/R2N1R2 b - - 0 18",
            "1k1r4/1pp2pp1/p1p5/2b1p1q1/2P1P1p1/P2P2P1/1P2QPKr/R2N1R2 w - - 1 19",
            "1k1r4/1pp2pp1/p1p5/2b1p1q1/2P1P1p1/P2P2P1/1P2QP1K/R2N1R2 b - - 0 19",
            "1k5r/1pp2pp1/p1p5/2b1p1q1/2P1P1p1/P2P2P1/1P2QP1K/R2N1R2 w - - 1 20",
            "1k5r/1pp2pp1/p1p5/2b1p1q1/2P1P1p1/P2P2P1/1P2QPK1/R2N1R2 b - - 2 20",
            "1k5r/1pp2pp1/p1p5/2b1p2q/2P1P1p1/P2P2P1/1P2QPK1/R2N1R2 w - - 3 21",
        ];

        let moves = [
            "e4", "e5", "Nf3", "Nc6", "Bb5", "a6", "Bxc6", "dxc6", "O-O", "Bg4", "h3", "h5", "d3",
            "Qf6", "Nc3", "Bd6", "Be3", "Ne7", "Qe2", "Ng6", "hxg4", "hxg4", "Ng5", "Nf4", "Bxf4",
            "Qxf4", "g3", "Qxg5", "Kg2", "O-O-O", "Nd1", "Kb8", "c4", "Bc5", "a3", "Rh2+", "Kxh2",
            "Rh8+", "Kg2", "Qh5",
        ];

        test_game(positions, moves);
    }
}
