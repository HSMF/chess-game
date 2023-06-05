use std::{
    fmt::Display,
    ops::{Add, Index, IndexMut},
    str::FromStr,
};

use anyhow::{anyhow, bail};
use sdl2::rect::Rect;

use crate::{ply::Ply, Piece, PieceKind, Player};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position {
    pub x: u8,
    pub y: u8,
}

impl Position {
    pub fn new(x: u8, y: u8) -> Position {
        Position { x, y }
    }

    pub fn try_new(x: u8, y: u8) -> Option<Position> {
        if x < 8 && y < 8 {
            Some(Position { x, y })
        } else {
            None
        }
    }

    pub(crate) fn from_physical(x: u32, y: u32) -> Self {
        use crate::graphics::WIDTH;
        let x = (x / WIDTH) as u8;
        let y = ((8 * WIDTH - y) / WIDTH) as u8;
        Self { x, y }
    }

    pub(crate) fn to_rect(self) -> Rect {
        use crate::graphics::WIDTH;

        Rect::new(
            self.x as i32 * WIDTH as i32,
            7 * WIDTH as i32 - self.y as i32 * WIDTH as i32,
            WIDTH,
            WIDTH,
        )
    }

    pub fn file(&self) -> u8 {
        self.x
    }

    pub fn rank(&self) -> u8 {
        self.y
    }

    #[allow(unused)]
    #[must_use]
    pub fn as_tuple(self) -> (u8, u8) {
        (self.x, self.y)
    }
}

impl Add<(i8, i8)> for Position {
    type Output = Option<Position>;

    fn add(self, (dx, dy): (i8, i8)) -> Self::Output {
        let x = self.x.checked_add_signed(dx)?;
        let y = self.y.checked_add_signed(dy)?;
        Position::try_new(x, y)
    }
}

impl From<(u8, u8)> for Position {
    fn from(value: (u8, u8)) -> Self {
        Self::new(value.0, value.1)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Position is invalid")]
pub struct InvalidPosition;

impl FromStr for Position {
    type Err = InvalidPosition;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        let x = chars.next().ok_or(InvalidPosition)?;
        let x = match x {
            'a'..='h' => x as u8 - b'a',
            _ => return Err(InvalidPosition),
        };
        let y = chars.next().ok_or(InvalidPosition)?;
        let y = match y {
            '1'..='8' => y as u8 - b'1',
            _ => return Err(InvalidPosition),
        };
        Ok(Position { x, y })
    }
}

#[derive(Debug, thiserror::Error)]
#[error("index is not contained by board")]
struct NotOnBoard;

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = if self.x < 8 { self.x + b'a' } else { b'?' } as char;
        let y = if self.y < 8 { self.y + b'1' } else { b'?' } as char;

        write!(f, "{x}{y}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Board {
    pub(super) fields: [Option<Piece>; 64],
}

impl Index<Position> for Board {
    type Output = Option<Piece>;

    fn index(&self, i: Position) -> &Self::Output {
        &self.fields[Self::index(i.x, i.y)]
    }
}

impl IndexMut<Position> for Board {
    fn index_mut(&mut self, i: Position) -> &mut Self::Output {
        &mut self.fields[Self::index(i.x, i.y)]
    }
}

impl Board {
    fn index(i: u8, j: u8) -> usize {
        let i = i as usize;
        let j = (7 - j) as usize;
        i + 8 * j
    }

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

    pub fn make_move(&mut self, ply: Ply) -> anyhow::Result<()> {
        match ply {
            Ply::Move { from, to, .. } => {
                let from = self[from].take().ok_or(anyhow!("no piece at {from:?}"))?;

                match &mut self[to] {
                    Some(x) if x.color == from.color => {
                        bail!("cannot move there, your own piece is there!")
                    }
                    Some(x) => {
                        *x = from;
                    }
                    r @ None => *r = Some(from),
                }

                Ok(())
            }
            _ => bail!("not yet implemented"),
        }
    }

    pub fn get(&self, pos: Position) -> Option<&Option<Piece>> {
        self.fields.get(Self::index(pos.x, pos.y))
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
