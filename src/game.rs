use std::{fmt::Display, str::FromStr};

use crate::ply::Ply;
use crate::{Piece, PieceKind, Player};
mod blockable_pieces;
mod board;
mod render;

pub use blockable_pieces::{BishopMove, KnightMove, Mover, PawnMove, QueenMove, RookMove};
pub use board::{Board, Position};
use either::Either;
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

    pub fn try_make_move(&mut self, ply: Ply) -> anyhow::Result<()> {
        match ply {
            Ply::Move {
                from,
                to,
                promoted_to,
            } => {
                let from_piece = match self.board[from] {
                    Some(p @ Piece { color, .. }) if color == self.to_move => p,
                    _ => anyhow::bail!("none of your pieces is on {from}"),
                };

                todo!()
            }
            Ply::Castle => todo!(),
            Ply::LongCastle => todo!(),
        }
    }

    pub fn possible_moves(
        &self,
        piece_pos: Position,
    ) -> Option<PieceMove> {
        self.board[piece_pos]?;

        Some(PieceMove::new(piece_pos, self))
    }

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
}
