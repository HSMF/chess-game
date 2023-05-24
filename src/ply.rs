use std::fmt::Display;

use anyhow::{anyhow, bail};
use either::Either;
use itertools::Itertools;
use nom::{
    branch::alt, bytes::complete::tag, character::complete::one_of, combinator::opt,
    sequence::tuple, IResult, Parser,
};

use crate::{
    game::{Board, Position},
    PieceKind, Player,
};

type IRes<'a, T> = IResult<&'a str, T>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ply {
    Move {
        from: Position,
        to: Position,
        promoted_to: Option<PieceKind>,
    },
    Castle,
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
struct RawPly {
    from: (Option<u8>, Option<u8>),
    to: (u8, u8),
    promoted_to: Option<PieceKind>,
    piece: PieceKind,
    captures: bool,
}

#[allow(dead_code)]
fn coord_to_string(pos: (u8, u8)) -> String {
    let file = b'a' + pos.0;
    let file = file as char;
    let rank = b'1' + pos.1;
    let rank = rank as char;

    format!("{file}{rank}")
}

impl Ply {
    #[allow(unused)]
    pub fn parse_pure(s: &str) -> anyhow::Result<Self> {
        let (s, (from, to, promoted_to)) = tuple((square, square, opt(promotable_piece)))(s)
            .map_err(|x| anyhow!("failed to parse: {x}"))?;
        if !s.is_empty() {
            bail!("trailing characters")
        }

        Ok(Ply::Move {
            from,
            to,
            promoted_to,
        })
    }

    pub fn parse_san(s: &str, board: &Board, player: Player) -> anyhow::Result<Self> {
        let (s, ply) = alt((pawn_push, pawn_capture, piece_move))(s)
            .map_err(|x| anyhow!("failed to parse: {x}"))?;
        if !s.is_empty() {
            bail!("trailing characters")
        }
        let RawPly {
            from,
            to,
            promoted_to,
            piece,
            captures,
        } = ply;

        if let (Some(file), Some(rank)) = from {
            return Ok(Ply::Move {
                from: (file, rank),
                to,
                promoted_to,
            });
        }

        fn unblocked<'a>(
            board: &'a Board,
            range: impl Iterator<Item = (u8, u8)> + 'a,
            piece: PieceKind,
            player: Player,
        ) -> impl Iterator<Item = (u8, u8)> + 'a {
            range
                .filter_map(move |pos| board.get(pos).and_then(|&p| p).map(|p| (pos, p)))
                .take_while(move |(_, p)| p.kind == piece && p.color == player)
                .map(|(pos, _)| pos)
                .take(1)
        }

        match piece {
            PieceKind::Pawn => {
                let range = if player == Player::White {
                    Either::Left((0..to.1).rev())
                } else {
                    Either::Right(to.1 + 1..8)
                };
                if captures {
                    let orig_coordinate = (
                        from.0.expect("this shouldn't not happen"),
                        from.1.unwrap_or(match player {
                            Player::White => to.1 - 1,
                            Player::Black => to.1 + 1,
                        }),
                    );

                    match board[orig_coordinate] {
                        Some(x) if x.kind == PieceKind::Pawn && x.color == player => {}
                        _ => bail!("no pawn at {orig_coordinate:?}"),
                    }
                    match board[to] {
                        Some(x) if x.color != player => {}
                        _ => bail!("cannot capture"),
                    }

                    let from = orig_coordinate;
                    return Ok(Ply::Move {
                        from,
                        to,
                        promoted_to,
                    });
                }

                if board[to].is_some() {
                    bail!("cannot push pawn there, square already occupied");
                }
                let from = range
                    .take(2)
                    .map(|i| (i, board[(to.0, i)]))
                    .filter_map(|(i, piece)| piece.map(|piece| (i, piece)))
                    .find(|(_, piece)| piece.kind == PieceKind::Pawn && piece.color == player)
                    .ok_or(anyhow!("no pawn in this file"))?
                    .0;

                let from = Position::new(to.0, from);

                    Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::Rook => {
                let left = unblocked(board, (0..to.0).rev().map(|x| (x, to.1)), piece, player);
                let right = unblocked(board, (to.0 + 1..8).map(|x| (x, to.1)), piece, player);
                let down = unblocked(board, (0..to.1).rev().map(|x| (to.0, x)), piece, player);
                let up = unblocked(board, (to.1 + 1..8).map(|x| (to.0, x)), piece, player);

                let possibilities = left.chain(right).chain(down).chain(up).collect_vec();

                if possibilities.is_empty() {
                    bail!("no rook could go there");
                }
                if possibilities.len() != 1 {
                    bail!("Ambiguous move. Too many rooks could move there.");
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::Knight => {
                let possibilities = [-1, 1]
                    .into_iter()
                    .cartesian_product([-2, 2].into_iter())
                    .chain([-2, 2].into_iter().cartesian_product([-1, 1].into_iter()))
                    .map(|(x, y)| (to.0.checked_add_signed(x), to.1.checked_add_signed(y)))
                    .filter_map(|(x, y)| x.zip(y))
                    .filter_map(|x| board.get(x).map(|piece| (x, piece)))
                    .filter_map(|(i, x)| x.map(|x| (i, x)))
                    .filter(|(_, x)| x.kind == PieceKind::Knight && x.color == player)
                    .map(|(i, _)| i)
                    .collect_vec();

                if possibilities.is_empty() {
                    bail!("no knight could jump there");
                }
                if possibilities.len() != 1 {
                    bail!("Ambiguous move. Too many knights could move there.");
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::Bishop => {
                let a = (0..8).map(|x| (to.0 + x, to.1 + x));
                let b = (0..8).filter_map(|x| to.0.checked_sub(x).map(|y| (y, to.1 + x)));
                let c = (0..8).filter_map(|x| to.1.checked_sub(x).map(|y| (to.0, y)));
                let d = (0..8).filter_map(|x| {
                    to.0.checked_sub(x)
                        .and_then(|y| to.1.checked_sub(x).map(|z| (y, z)))
                });

                let a = unblocked(board, a, piece, player);
                let b = unblocked(board, b, piece, player);
                let c = unblocked(board, c, piece, player);
                let d = unblocked(board, d, piece, player);

                let possibilities = a.chain(b).chain(c).chain(d).collect_vec();

                if possibilities.is_empty() {
                    bail!("no bishop could jump there");
                }
                if possibilities.len() != 1 {
                    bail!("Ambiguous move. Too many bishops could move there.");
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::Queen => {
                let left = unblocked(board, (0..to.0).rev().map(|x| (x, to.1)), piece, player);
                let right = unblocked(board, (to.0 + 1..8).map(|x| (x, to.1)), piece, player);
                let down = unblocked(board, (0..to.1).rev().map(|x| (to.0, x)), piece, player);
                let up = unblocked(board, (to.1 + 1..8).map(|x| (to.0, x)), piece, player);
                let a = (1..8).map(|x| (to.0 + x, to.1 + x));
                let b = (1..8).filter_map(|x| to.0.checked_sub(x).map(|y| (y, to.1 + x)));
                let c = (1..8).filter_map(|x| to.1.checked_sub(x).map(|y| (to.0, y)));
                let d = (1..8).filter_map(|x| {
                    to.0.checked_sub(x)
                        .and_then(|y| to.1.checked_sub(x).map(|z| (y, z)))
                });

                let a = unblocked(board, a, piece, player);
                let b = unblocked(board, b, piece, player);
                let c = unblocked(board, c, piece, player);
                let d = unblocked(board, d, piece, player);

                let possibilities = a
                    .chain(b)
                    .chain(c)
                    .chain(d)
                    .chain(left)
                    .chain(right)
                    .chain(down)
                    .chain(up)
                    .collect_vec();

                if possibilities.is_empty() {
                    bail!("no queen could move there");
                }
                if possibilities.len() != 1 {
                    bail!("Ambiguous move. Too many queens could move there.");
                }

                let from = possibilities[0];

                Ok(Ply::Move {
                    from,
                    to,
                    promoted_to,
                })
            }
            PieceKind::King => {
                let possibilities = (-1..1)
                    .cartesian_product(-1..1)
                    .filter(|&x| x != (0, 0))
                    .filter_map(|(l, r)| {
                        to.0.checked_add_signed(l)
                            .and_then(|x| to.1.checked_add_signed(r).map(|y| (x, y)))
                    })
                    .filter_map(move |pos| board.get(pos).and_then(|&p| p).map(|p| (pos, p)))
                    .filter(|(_, x)| x.kind == PieceKind::King && x.color == player)
                    .map(|(x, _)| x)
                    .collect_vec();

                if possibilities.is_empty() {
                    bail!("no king could move there");
                }
                if possibilities.len() != 1 {
                    bail!("Ambiguous move. Too many kings could move there.");
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

fn square(s: &str) -> IRes<(u8, u8)> {
    let (s, (x, y)) = tuple((get_file, get_rank))(s)?;

    Ok((s, (x, y)))
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
    Square((u8, u8)),
}

fn piece_move(s: &str) -> IRes<RawPly> {
    let (s, who) = piece(s)?;

    fn tail(s: &str) -> IRes<(bool, (u8, u8))> {
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
        Some(Disambiguator::Square((file, rank))) => (Some(file), Some(rank)),
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
        let board = Board::new();
        let player = Player::White;

        let ply = Ply::parse_san("e4", &board, player).unwrap();
        let expected = Ply::parse_pure("e2e4").unwrap();

        assert_eq!(ply, expected);
    }

    #[test]
    fn e5() {
        let board = Board::new();
        let player = Player::Black;

        let ply = Ply::parse_san("e5", &board, player).unwrap();
        let expected = Ply::parse_pure("e7e5").unwrap();

        assert_eq!(ply, expected);
    }

    #[test]
    fn e4d5exd5() {
        let mut board = Board::new();

        board
            .make_move(Ply::parse_san("e4", &board, Player::White).unwrap())
            .unwrap();
        board
            .make_move(Ply::parse_san("d5", &board, Player::Black).unwrap())
            .unwrap();

        let ply = Ply::parse_san("exd5", &board, Player::White).unwrap();
        let expected = Ply::parse_pure("e4d5").unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn e4d5dxe4() {
        let mut board = Board::new();

        board
            .make_move(Ply::parse_san("e4", &board, Player::White).unwrap())
            .unwrap();
        board
            .make_move(Ply::parse_san("d5", &board, Player::Black).unwrap())
            .unwrap();

        let ply = Ply::parse_san("dxe4", &board, Player::Black).unwrap();
        let expected = Ply::parse_pure("d5e4").unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn nf3() {
        let board = Board::new();
        let player = Player::White;

        let expected = Ply::parse_pure("g1f3").unwrap();
        let ply = Ply::parse_san("Nf3", &board, player).unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("Ng1f3", &board, player).unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("Ngf3", &board, player).unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("N1f3", &board, player).unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn e4_d5_ke2() {
        let mut board = Board::new();

        board
            .make_move(Ply::parse_san("e4", &board, Player::White).unwrap())
            .unwrap();
        board
            .make_move(Ply::parse_san("d5", &board, Player::Black).unwrap())
            .unwrap();

        let ply = Ply::parse_san("Ke2", &board, Player::White).unwrap();
        let expected = Ply::parse_pure("e1e2").unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("K1e2", &board, Player::White).unwrap();
        assert_eq!(ply, expected);
        let ply = Ply::parse_san("Kee2", &board, Player::White).unwrap();
        assert_eq!(ply, expected);
        let ply = Ply::parse_san("Ke1e2", &board, Player::White).unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn a4_d5_ra3() {
        let mut board = Board::new();

        board
            .make_move(Ply::parse_san("a4", &board, Player::White).unwrap())
            .unwrap();
        board
            .make_move(Ply::parse_san("e5", &board, Player::Black).unwrap())
            .unwrap();

        let ply = Ply::parse_san("Ra3", &board, Player::White).unwrap();
        let expected = Ply::parse_pure("a1a3").unwrap();
        assert_eq!(ply, expected);

        let ply = Ply::parse_san("Raa3", &board, Player::White).unwrap();
        assert_eq!(ply, expected);
        let ply = Ply::parse_san("R1a3", &board, Player::White).unwrap();
        assert_eq!(ply, expected);
        let ply = Ply::parse_san("Ra1a3", &board, Player::White).unwrap();
        assert_eq!(ply, expected);
    }

    #[test]
    fn hm() {
        let board: Board = "
r___ k__r
_pp_ ___p
_p__ b___
____ Q_p_
____ __P_
N_P_ _q__
PP__ ___P
R___ __K_"
            .parse()
            .unwrap();

        assert!(Ply::parse_san("Rh6", &board, Player::Black).is_err());

        let ply = Ply::parse_san("Rd1", &board, Player::White).unwrap();
        let expected = Ply::parse_pure("a1d1").unwrap();

        assert_eq!(ply, expected);

        assert!(Ply::parse_san("R8h6", &board, Player::Black).is_err());
    }
}
