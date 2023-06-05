use std::{fmt::Display, str::FromStr};

use crate::ply::Ply;
use crate::{Piece, PieceKind, Player};
mod blockable_pieces;
mod board;
mod render;

pub use blockable_pieces::{BishopMove, KnightMove, Mover, PawnMove, QueenMove, RookMove};
pub use board::{Board, Position};
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

#[derive(Debug, PartialEq, Eq)]
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
    en_passant_sq: Option<Position>,
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

#[derive(Debug, thiserror::Error)]
#[error("{ply}")]
pub struct MoveError {
    ply: Ply,
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

    /// Attempts to make a move, returning Err if the given move was not valid
    pub fn try_make_move(&mut self, ply: Ply) -> anyhow::Result<()> {
        let mut reset_counter = false;
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

                tinyvec::array_vec!([(Position, Position); 2] => (from, to))
            }
            Ply::Castle => {
                reset_counter = true;
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

                anyhow::ensure!(possible_moves.contains(&king_to), "cannot castle to {king_to}");


                let rook_from = Position::new(7, home_row);
                let rook_to = Position::new(5, home_row);



                tinyvec::array_vec!([(Position, Position); 2] => (king_from, king_to), (rook_from, rook_to))
            }
            Ply::LongCastle => {
                reset_counter = true;
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

                anyhow::ensure!(possible_moves.contains(&king_to), "cannot castle to {king_to}");


                let rook_from = Position::new(0, home_row);
                let rook_to = Position::new(3, home_row);

                tinyvec::array_vec!([(Position, Position); 2] => (king_from, king_to), (rook_from, rook_to))
            }
        };

        let mut old_state = tinyvec::array_vec!([(Position, Piece); 4]);
        for (from, to) in moves {
            // make a backup
            let moving_piece = self.board[from].expect("must have a piece here");
            old_state.push((from, moving_piece));
            if let Some(piece) = self.board[to] {
                old_state.push((to, piece));
            }
            self.board[to] = Some(moving_piece);
            self.board[from] = None;
        }

        // check if the turn was valid, else revert

        if reset_counter {
            self.halfmove_clock = 0;
        }
        if self.to_move == Player::Black {
            self.fullmove_clock += 1;
        }

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

    macro_rules! make_move {
        ($name:ident, $the_move:literal :: $before_fen:literal -> $after_fen:literal) => {
            #[test]
            fn $name() {
                let mut before: Game = $before_fen.parse().expect("should be a valid fen string");
                let after: Game = $after_fen.parse().expect("should be a valid fen string");
                let the_move = Ply::parse_pure($the_move).expect("should be a valid pure move");
                before.try_make_move(the_move).expect("was a valid move");

                assert_eq!(before, after);
            }
        };
    }

    make_move!(first, "e2e4" :: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                             -> "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1");
    make_move!(second, "d7d5" :: "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
                             ->  "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2");
}
