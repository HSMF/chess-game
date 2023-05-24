use std::{
    fmt::Display,
    ops::{Index, IndexMut},
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
}

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

impl Index<(u8, u8)> for Board {
    type Output = Option<Piece>;

    fn index(&self, (i, j): (u8, u8)) -> &Self::Output {
        &self.fields[Self::index(i, j)]
    }
}

impl IndexMut<(u8, u8)> for Board {
    fn index_mut(&mut self, (i, j): (u8, u8)) -> &mut Self::Output {
        &mut self.fields[Self::index(i, j)]
    }
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
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
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

    pub fn get(&self, (i, j): (u8, u8)) -> Option<&<Self as Index<(u8, u8)>>::Output> {
        self.fields.get(Self::index(i, j))
    }

    pub fn get_pos(&self, pos: Position) -> Option<&Option<Piece>> {
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
            write!(f, "{} ", i + 1).unwrap();
            if let Some(p) = self[(0, i)] {
                write!(f, "| {} |", p)?;
            } else {
                write!(f, "|   |")?;
            }
            for j in 1..8 {
                match self[(j, i)] {
                    Some(p) => write!(f, " {} |", p).unwrap(),
                    None => write!(f, "   |").unwrap(),
                }
            }
            writeln!(f).unwrap();
            writeln!(f, "  +---+---+---+---+---+---+---+---+").unwrap();
        }

        Ok(())
    }
}
