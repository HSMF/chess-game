use std::{
    fmt::Display,
    hash::Hash,
    ops::{Index, IndexMut},
};

use crate::{Piece, PieceKind, Position};

/// A chess board. Indexable via [`Position`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    /// returnrs a pretty formatter for the board
    pub fn pretty(&self) -> PrettyBoard {
        PrettyBoard { inner: self }
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

/// wrapper with a pretty board repr
#[derive(Debug)]
pub struct PrettyBoard<'a> {
    inner: &'a Board,
}

impl<'a> Display for PrettyBoard<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "    a   b   c   d   e   f   g   h")?;
        writeln!(f, "  +---+---+---+---+---+---+---+---+")?;

        for i in (0..8).rev() {
            write!(f, "{} ", i + 1)?;
            if let Some(p) = self.inner[Position::new(0, i)] {
                write!(f, "| {} |", p)?;
            } else {
                write!(f, "|   |")?;
            }
            for j in 1..8 {
                match self.inner[Position::new(j, i)] {
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

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in (0..8).rev() {
            let mut skip = 0;
            for x in 0..8 {
                match self.fields[Self::index(x, y)] {
                    Some(p) => {
                        if skip != 0 {
                            write!(f, "{skip}")?;
                        }
                        skip = 0;
                        write!(f, "{p}")?;
                    }
                    None => skip += 1,
                }
            }
            if skip != 0 {
                write!(f, "{skip}")?;
            }
            if y != 0 {
                write!(f, "/")?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pieces::*;
    mod pieces {
        #![allow(non_upper_case_globals, unused)]
        use crate::{PieceKind, game::Piece};
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
    fn to_string_starting_pos() {
        let board = Board::default();
        assert_eq!(
            board.to_string(),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        )
    }

    #[test]
    fn to_string_random() {
        #[rustfmt::skip]
        let board = Board {
            fields:
            [
                x,x,k,x,x,x,x,x,
                x,x,p,x,x,p,b,x,
                x,x,x,p,x,x,p,p,
                x,x,x,P,x,x,x,x,
                p,x,P,P,x,B,x,x,
                N,x,x,x,x,P,x,x,
                x,r,x,x,x,K,P,P,
                x,x,x,x,x,x,x,x,
            ],
        };

        assert_eq!(board.to_string(), "2k5/2p2pb1/3p2pp/3P4/p1PP1B2/N4P2/1r3KPP/8")
    }
}
