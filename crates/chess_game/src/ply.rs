use std::fmt::Display;

use either::Either;
use itertools::Itertools;
use nom::{
    branch::alt, bytes::complete::tag, character::complete::one_of, combinator::opt,
    sequence::tuple, IResult, Parser,
};

use crate::{
    game::{BishopMove, HasPieceKind, KingMove, KnightMove, Mover, QueenMove, RookMove},
    Game, PieceKind, Player, Position,
};

type IRes<'a, T> = IResult<&'a str, T>;

/// represents a half-move (i.e. a move of only one player).
///
/// see [this](https://en.wikipedia.org/wiki/Ply_(game_theory)) for the difference to "move"
///
///
/// ## Examples
///
/// ```
/// # use chess_game::{Ply, Player, Game};
/// let game = Game::new();
/// let ply = Ply::parse_san("e4", &game).unwrap();
///
/// assert_eq!(ply, Ply::parse_pure("e2e4").unwrap());
/// assert_eq!(ply.is_move(), true);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Ply {
    /// A regular move.
    Move {
        /// the starting position
        from: Position,
        /// the target position
        to: Position,
        /// if the piece moving is a pawn and it reaches the other end of the board, it may promote
        /// to a better piece
        promoted_to: Option<PieceKind>,
    },
    /// Castling king's side.
    #[default]
    Castle,
    /// Castling queens side.
    LongCastle,
}

impl Display for Ply {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ply::Move {
                from,
                to,
                promoted_to,
            } => {
                write!(f, "{}{}", from, to)?;
                if let Some(to) = promoted_to {
                    write!(f, "={}", to)?;
                }
            }
            Ply::Castle => write!(f, "O-O")?,
            Ply::LongCastle => write!(f, "O-O-O")?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct RawPly {
    from: (Option<u8>, Option<u8>),
    to: Position,
    promoted_to: Option<PieceKind>,
    piece: PieceKind,
    captures: bool,
}

/// Error that arises from [`Ply::parse_pure`]
#[derive(Debug, thiserror::Error)]
pub enum ParsePureError<'a> {
    /// the input had invalid format
    #[error("invalid format: {0}")]
    Format(nom::Err<nom::error::Error<&'a str>>),
    /// the input was parsed correctly but has trailing characters
    #[error("unexpected suffix: {0:?}")]
    TrailingChars(&'a str),
}

/// Error that arises from [`Ply::parse_san`]
#[derive(Debug, thiserror::Error)]
pub enum ParseSanError<'a> {
    /// the input had invalid format
    #[error("invalid format: {0}")]
    Format(nom::Err<nom::error::Error<&'a str>>),
    /// the input was parsed correctly but has trailing characters
    #[error("unexpected suffix: {0:?}")]
    TrailingChars(&'a str),
    /// there was no pawn at the position, when trying to disambiguate pawn movements
    #[error("no pawn at {0}")]
    NoPawnAt(Position),
    /// the pawn was found but cannot capture
    #[error("pawn cannot capture")]
    PawnCannotCapture,
    /// the target square already contains a piece of the same color
    #[error("space is already occupied")]
    SpaceOccupied,
    /// there was no pawn in that file (when trying to push)
    #[error("no pawn in that file")]
    NoPawnInFile,
    /// no corresponding piece was found that could move there
    #[error("no {0:?} was found")]
    NoPieceFound(PieceKind),
    /// too many corresponding pieces were found that could move there. Therefore the move isn't
    /// unambiguous
    #[error("too many {0:?}s were found")]
    TooManyPiecesFound(PieceKind),
}

pub(crate) type WipPly = Either<RawPly, Ply>;
pub(crate) fn san_ply(s: &str) -> IRes<WipPly> {
    let (s, (m, _)) = tuple((
        alt((
            pawn_push.map(Either::Left),
            pawn_capture.map(Either::Left),
            piece_move.map(Either::Left),
            long_castle.map(Either::Right),
            castle.map(Either::Right),
        )),
        opt(one_of("#+")),
    ))(s)?;
    Ok((s, m))
}

impl Ply {
    /// Parses a "pure" representation of a ply, i.e. either `O-O` / `O-O-O` for short and long castle,
    /// respectively, or `<from square><to square>[promoted to]`
    ///
    /// Note that this is extremely basic, so it does not check if the move makes any sense at all
    ///
    /// ## Examples
    ///
    /// ```
    /// # use chess_game::Ply;
    /// assert_eq!(Ply::parse_pure("e2e4").unwrap(), Ply::Move {
    ///     from: "e2".parse().unwrap(),
    ///     to: "e4".parse().unwrap(),
    ///     promoted_to: None
    /// })
    /// ```
    pub fn parse_pure(s: &str) -> Result<Self, ParsePureError> {
        if s == "O-O" {
            return Ok(Ply::Castle);
        }
        if s == "O-O-O" {
            return Ok(Ply::LongCastle);
        }
        let (s, (from, to, promoted_to)) =
            tuple((square, square, opt(promotable_piece)))(s).map_err(ParsePureError::Format)?;
        if !s.is_empty() {
            return Err(ParsePureError::TrailingChars(s));
        }

        Ok(Ply::Move {
            from,
            to,
            promoted_to,
        })
    }

    /// parses a SAN ply identifier. Does not exhaustively verify whether the move is valid
    ///
    /// ## Examples
    /// ```
    /// # use chess_game::{Game, Ply};
    /// let game = Game::new();
    /// let ply = Ply::parse_san("e4", &game).unwrap();
    /// assert_eq!(ply, Ply::parse_pure("e2e4").unwrap());
    /// ```
    pub fn parse_san<'a>(s: &'a str, game: &Game) -> Result<Self, ParseSanError<'a>> {
        if s == "O-O" {
            return Ok(Ply::Castle);
        }
        if s == "O-O-O" {
            return Ok(Ply::LongCastle);
        }
        let (s, ply) = san_ply(s).map_err(ParseSanError::Format)?;

        if !s.is_empty() {
            return Err(ParseSanError::TrailingChars(s));
        }

        let ply = match ply {
            Either::Left(rply) => rply,
            Either::Right(ply) => return Ok(ply),
        };

        Self::resolve_san_ply(ply, game)
    }

    pub(crate) fn resolve_san_ply<'a>(ply: RawPly, game: &Game) -> Result<Self, ParseSanError<'a>> {
        let player = game.player_to_move();
        let RawPly {
            from,
            to,
            promoted_to,
            piece,
            captures,
        } = ply;

        if let (Some(file), Some(rank)) = from {
            return Ok(Ply::Move {
                from: Position::new(file, rank),
                to,
                promoted_to,
            });
        }

        fn find_moves<'a, M: Mover<'a> + HasPieceKind>(
            game: &'a Game,
            player: Player,
            to: Position,
            from: (Option<u8>, Option<u8>),
        ) -> Vec<Position> {
            game.pieces()
                .filter(|(_, piece)| piece.kind() == M::kind() && piece.color == player)
                .map(|(pos, _)| (pos, M::new_with_color(pos, game, player)))
                .filter_map(
                    |(pos, mut piece)| {
                        if piece.contains(&to) {
                            Some(pos)
                        } else {
                            None
                        }
                    },
                )
                .filter(|pos| from.0.map(|x| pos.x() == x).unwrap_or(true))
                .filter(|pos| from.1.map(|y| pos.y() == y).unwrap_or(true))
                .collect_vec()
        }

        match piece {
            PieceKind::Pawn => {
                let range = if player == Player::White {
                    Either::Left((0..to.y()).rev())
                } else {
                    Either::Right(to.y() + 1..8)
                };
                if captures {
                    let orig_coordinate = Position::new(
                        from.0.expect("this shouldn't not happen"),
                        from.1.unwrap_or(match player {
                            Player::White => to.y() - 1,
                            Player::Black => to.y() + 1,
                        }),
                    );

                    match game[orig_coordinate] {
                        Some(x) if x.kind == PieceKind::Pawn && x.color == player => {}
                        _ => return Err(ParseSanError::NoPawnAt(orig_coordinate)),
                    }

                    match game[to] {
                        Some(x) if x.color != player => {}
                        _ if Some(to) == game.en_passant_sq => {}
                        Some(_) => return Err(ParseSanError::SpaceOccupied),
                        _ => return Err(ParseSanError::PawnCannotCapture),
                    }

                    let from = orig_coordinate;
                    return Ok(Ply::Move {
                        from,
                        to,
                        promoted_to,
                    });
                }

                if game[to].is_some() {
                    return Err(ParseSanError::SpaceOccupied);
                }
                let from = range
                    .take(2)
                    .map(|i| (i, game[Position::new(to.x(), i)]))
                    .filter_map(|(i, piece)| piece.map(|piece| (i, piece)))
                    .find(|(_, piece)| piece.kind == PieceKind::Pawn && piece.color == player)
                    .ok_or(ParseSanError::NoPawnInFile)?
                    .0;

                let from = Position::new(to.x(), from);

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::Rook => {
                let possibilities = find_moves::<RookMove>(game, player, to, from);

                if possibilities.is_empty() {
                    return Err(ParseSanError::NoPieceFound(PieceKind::Rook));
                }
                if possibilities.len() != 1 {
                    return Err(ParseSanError::TooManyPiecesFound(PieceKind::Rook));
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::Knight => {
                let possibilities = find_moves::<KnightMove>(game, player, to, from);

                if possibilities.is_empty() {
                    return Err(ParseSanError::NoPieceFound(PieceKind::Knight));
                }
                if possibilities.len() != 1 {
                    return Err(ParseSanError::TooManyPiecesFound(PieceKind::Knight));
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::Bishop => {
                let possibilities = find_moves::<BishopMove>(game, player, to, from);

                if possibilities.is_empty() {
                    return Err(ParseSanError::NoPieceFound(PieceKind::Bishop));
                }
                if possibilities.len() != 1 {
                    return Err(ParseSanError::TooManyPiecesFound(PieceKind::Bishop));
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::Queen => {
                let possibilities = find_moves::<QueenMove>(game, player, to, from);

                if possibilities.is_empty() {
                    return Err(ParseSanError::NoPieceFound(PieceKind::Queen));
                }
                if possibilities.len() != 1 {
                    return Err(ParseSanError::TooManyPiecesFound(PieceKind::Queen));
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::King => {
                let possibilities = find_moves::<KingMove>(game, player, to, from);

                if possibilities.is_empty() {
                    return Err(ParseSanError::NoPieceFound(PieceKind::King));
                }
                if possibilities.len() != 1 {
                    return Err(ParseSanError::TooManyPiecesFound(PieceKind::Queen));
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
        }
    }

    /// Returns `true` if the ply is [`Move`].
    ///
    /// [`Move`]: Ply::Move
    #[must_use]
    pub fn is_move(&self) -> bool {
        matches!(self, Self::Move { .. })
    }

    /// Returns `true` if the ply is [`Castle`].
    ///
    /// [`Castle`]: Ply::Castle
    #[must_use]
    pub fn is_castle(&self) -> bool {
        matches!(self, Self::Castle)
    }

    /// Returns `true` if the ply is [`LongCastle`].
    ///
    /// [`LongCastle`]: Ply::LongCastle
    #[must_use]
    pub fn is_long_castle(&self) -> bool {
        matches!(self, Self::LongCastle)
    }
}

fn get_file(s: &str) -> IRes<u8> {
    let (s, x) = one_of("abcdefgh")(s)?;
    let x = x as u8 - b'a';
    Ok((s, x))
}

fn get_rank(s: &str) -> IRes<u8> {
    let (s, x) = one_of("12345678")(s)?;
    let x = x as u8 - b'1';
    Ok((s, x))
}

fn square(s: &str) -> IRes<Position> {
    let (s, (x, y)) = tuple((get_file, get_rank))(s)?;

    Ok((s, Position::new(x, y)))
}

fn promotable_piece(s: &str) -> IRes<PieceKind> {
    let (s, p) = one_of("QRBN")(s)?;

    use PieceKind::*;
    let piece = match p {
        'Q' => Queen,
        'R' => Rook,
        'B' => Bishop,
        'N' => Knight,
        _ => unreachable!(),
    };

    Ok((s, piece))
}

fn piece(s: &str) -> IRes<PieceKind> {
    let (s, p) = one_of("QRBNK")(s)?;

    use PieceKind::*;
    let piece = match p {
        'Q' => Queen,
        'R' => Rook,
        'B' => Bishop,
        'N' => Knight,
        'K' => King,
        _ => unreachable!(),
    };

    Ok((s, piece))
}

fn promotion(s: &str) -> IRes<PieceKind> {
    let (s, _) = tag("=")(s)?;

    let (s, to) = promotable_piece(s)?;

    Ok((s, to))
}

fn captures(s: &str) -> IRes<bool> {
    let (s, capt) = opt(tag("x"))(s)?;
    Ok((s, capt.is_some()))
}

#[derive(Debug)]
enum Disambiguator {
    Rank(u8),
    File(u8),
    Square(Position),
}

fn piece_move(s: &str) -> IRes<RawPly> {
    let (s, who) = piece(s)?;

    fn tail(s: &str) -> IRes<(bool, Position)> {
        tuple((captures, square))(s)
    }

    let (s, (disambiguator, (captures, to))) = alt((
        tuple((
            alt((
                square.map(Disambiguator::Square),
                get_rank.map(Disambiguator::Rank),
                get_file.map(Disambiguator::File),
            ))
            .map(Some),
            tail,
        )),
        tail.map(|x| (None, x)),
    ))(s)?;

    let from = match disambiguator {
        Some(Disambiguator::Rank(rank)) => (None, Some(rank)),
        Some(Disambiguator::File(file)) => (Some(file), None),
        Some(Disambiguator::Square(pos)) => (Some(pos.x()), Some(pos.y())),
        None => (None, None),
    };

    Ok((
        s,
        RawPly {
            from,
            to,
            promoted_to: None,
            piece: who,
            captures,
        },
    ))
}

fn pawn_capture(s: &str) -> IRes<RawPly> {
    let (s, from_file) = get_file(s)?;
    let (s, from_rank) = opt(get_rank)(s)?;
    let (s, _) = tag("x")(s)?;
    let (s, to) = square(s)?;
    let (s, promoted_to) = opt(promotion)(s)?;

    Ok((
        s,
        RawPly {
            to,
            from: (Some(from_file), from_rank),
            promoted_to,
            piece: PieceKind::Pawn,
            captures: true,
        },
    ))
}

fn castle(s: &str) -> IRes<Ply> {
    let (s, _) = tag("O-O")(s)?;
    Ok((s, Ply::Castle))
}

fn long_castle(s: &str) -> IRes<Ply> {
    let (s, _) = tag("O-O-O")(s)?;
    Ok((s, Ply::LongCastle))
}

fn pawn_push(s: &str) -> IRes<RawPly> {
    let (s, (to, promoted_to)) = tuple((square, opt(promotion)))(s)?;

    Ok((
        s,
        RawPly {
            from: (None, None),
            to,
            promoted_to,
            piece: PieceKind::Pawn,
            captures: false,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn e4() {
        let game = Game::new();

        let ply = Ply::parse_san("e4", &game).unwrap();
        let expected = Ply::parse_pure("e2e4").unwrap();

        assert_eq!(ply, expected);
    }

    #[test]
    fn e5() {
        let mut game = Game::new();
        *game.to_move_mut() = Player::Black;

        let ply = Ply::parse_san("e5", &game).unwrap();
        let expected = Ply::parse_pure("e7e5").unwrap();

        assert_eq!(ply, expected);
    }

    #[test]
    fn e4d5exd5() {
        let mut game = Game::new();

        game.try_make_move(Ply::parse_san("e4", &game).unwrap())
            .unwrap();
        game.try_make_move(Ply::parse_san("d5", &game).unwrap())
            .unwrap();

        let ply = Ply::parse_san("exd5", &game).unwrap();
        let expected = Ply::parse_pure("e4d5").unwrap();

        assert_eq!(ply, expected);
    }

    #[test]
    fn e4d5dxe4() {
        let mut game = Game::new();

        game.try_make_move(Ply::parse_san("e4", &game).unwrap())
            .unwrap();
        game.try_make_move(Ply::parse_san("d5", &game).unwrap())
            .unwrap();
        *game.to_move_mut() = Player::Black;
        let ply = Ply::parse_san("dxe4", &game).unwrap();
        let expected = Ply::parse_pure("d5e4").unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn nf3() {
        let game = Game::new();

        let expected = Ply::parse_pure("g1f3").unwrap();
        let ply = Ply::parse_san("Nf3", &game).unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("Ng1f3", &game).unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("Ngf3", &game).unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("N1f3", &game).unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn e4_d5_ke2() {
        let mut game = Game::new();

        game.try_make_move(Ply::parse_san("e4", &game).unwrap())
            .unwrap();
        game.try_make_move(Ply::parse_san("d5", &game).unwrap())
            .unwrap();

        let ply = Ply::parse_san("Ke2", &game).unwrap();
        let expected = Ply::parse_pure("e1e2").unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("K1e2", &game).unwrap();
        assert_eq!(ply, expected);
        let ply = Ply::parse_san("Kee2", &game).unwrap();
        assert_eq!(ply, expected);
        let ply = Ply::parse_san("Ke1e2", &game).unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn a4_d5_ra3() {
        let mut game = Game::new();

        game.try_make_move(Ply::parse_san("a4", &game).unwrap())
            .unwrap();
        game.try_make_move(Ply::parse_san("e5", &game).unwrap())
            .unwrap();

        let ply = Ply::parse_san("Ra3", &game).unwrap();
        let expected = Ply::parse_pure("a1a3").unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("Raa3", &game).unwrap();
        assert_eq!(ply, expected);
        let ply = Ply::parse_san("R1a3", &game).unwrap();
        assert_eq!(ply, expected);
        let ply = Ply::parse_san("Ra1a3", &game).unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn hm() {
        let game: Game = "r3k2r/1pp4p/1p2b3/4Q1p1/6P1/N1P2q2/PP5P/R5K1 w - - 0 1"
            .parse()
            .unwrap();

        assert!(Ply::parse_san("Rh6", &game).is_err());

        let ply = Ply::parse_san("Rd1", &game).unwrap();
        let expected = Ply::parse_pure("a1d1").unwrap();

        assert_eq!(ply, expected);

        assert!(Ply::parse_san("R8h6", &game).is_err());
    }
}
