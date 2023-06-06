use std::{
    fmt::Display,
    ops::{Index, IndexMut},
    str::FromStr,
};

use anyhow::anyhow;

use crate::{Piece, PieceKind, Player, Position};

/// A chess board. Indexable via [`Position`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Board {
    pub(super) fields: [Option<Piece>; 64],
}

impl Index<Position> for Board {
    type Output = Option<Piece>;

    fn index(&self, i: Position) -> &Self::Output {
        &self.fields[Self::index(i.x(), i.y())]
    }
}

impl IndexMut<Position> for Board {
    fn index_mut(&mut self, i: Position) -> &mut Self::Output {
        &mut self.fields[Self::index(i.x(), i.y())]
    }
}

impl Board {
    fn index(i: u8, j: u8) -> usize {
        let i = i as usize;
        let j = (7 - j) as usize;
        i + 8 * j
    }

    /// Sets up a new board in the standard way.
    pub fn new() -> Self {
        use PieceKind::*;
        Board {
            #[rustfmt::skip]
            fields: [
                Some(Piece::new_black(Rook)),
                Some(Piece::new_black(Knight)),
                Some(Piece::new_black(Bishop)),
                Some(Piece::new_black(Queen)),
                Some(Piece::new_black(King)),
                Some(Piece::new_black(Bishop)),
                Some(Piece::new_black(Knight)),
                Some(Piece::new_black(Rook)),
                Some(Piece::new_black(Pawn)),
                Some(Piece::new_black(Pawn)),
                Some(Piece::new_black(Pawn)),
                Some(Piece::new_black(Pawn)),
                Some(Piece::new_black(Pawn)),
                Some(Piece::new_black(Pawn)),
                Some(Piece::new_black(Pawn)),
                Some(Piece::new_black(Pawn)),
                None, None, None, None, None, None, None, None,
                None, None, None, None, None, None, None, None,
                None, None, None, None, None, None, None, None,
                None, None, None, None, None, None, None, None,
                Some(Piece::new_white(Pawn)),
                Some(Piece::new_white(Pawn)),
                Some(Piece::new_white(Pawn)),
                Some(Piece::new_white(Pawn)),
                Some(Piece::new_white(Pawn)),
                Some(Piece::new_white(Pawn)),
                Some(Piece::new_white(Pawn)),
                Some(Piece::new_white(Pawn)),
                Some(Piece::new_white(Rook)),
                Some(Piece::new_white(Knight)),
                Some(Piece::new_white(Bishop)),
                Some(Piece::new_white(Queen)),
                Some(Piece::new_white(King)),
                Some(Piece::new_white(Bishop)),
                Some(Piece::new_white(Knight)),
                Some(Piece::new_white(Rook)),
            ],
        }
    }
}

impl FromStr for Board {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use PieceKind::*;
        let mut fields = [None; 64];
        for (i, piece) in s
            .chars()
            .filter(|x| !x.is_whitespace())
            .map(|x| match x {
                'P' => Ok(Some(Piece {
                    kind: Pawn,
                    color: Player::White,
                })),
                'R' => Ok(Some(Piece {
                    kind: Rook,
                    color: Player::White,
                })),
                'N' => Ok(Some(Piece {
                    kind: Knight,
                    color: Player::White,
                })),
                'B' => Ok(Some(Piece {
                    kind: Bishop,
                    color: Player::White,
                })),
                'Q' => Ok(Some(Piece {
                    kind: Queen,
                    color: Player::White,
                })),
                'K' => Ok(Some(Piece {
                    kind: King,
                    color: Player::White,
                })),

                'p' => Ok(Some(Piece {
                    kind: Pawn,
                    color: Player::Black,
                })),
                'r' => Ok(Some(Piece {
                    kind: Rook,
                    color: Player::Black,
                })),
                'n' => Ok(Some(Piece {
                    kind: Knight,
                    color: Player::Black,
                })),
                'b' => Ok(Some(Piece {
                    kind: Bishop,
                    color: Player::Black,
                })),
                'q' => Ok(Some(Piece {
                    kind: Queen,
                    color: Player::Black,
                })),
                'k' => Ok(Some(Piece {
                    kind: King,
                    color: Player::Black,
                })),

                '_' => Ok(None),
                x => Err(anyhow!("invalid symbol: {x}")),
            })
            .enumerate()
        {
            let piece = piece?;
            fields[i] = piece;
        }

        Ok(Board { fields })
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "    a   b   c   d   e   f   g   h").unwrap();
        writeln!(f, "  +---+---+---+---+---+---+---+---+").unwrap();

        for i in (0..8).rev() {
            write!(f, "{} ", i + 1)?;
            if let Some(p) = self[Position::new(0, i)] {
                write!(f, "| {} |", p)?;
            } else {
                write!(f, "|   |")?;
            }
            for j in 1..8 {
                match self[Position::new(j, i)] {
                    Some(p) => write!(f, " {} |", p)?,
                    None => write!(f, "   |")?,
                }
            }
            writeln!(f)?;
            writeln!(f, "  +---+---+---+---+---+---+---+---+")?;
        }

        Ok(())
    }
}
