pub mod game;
mod ply;
pub mod graphics;
mod position;
pub use position::Position;

use std::fmt::Display;

pub use game::Game;
pub use ply::Ply;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PieceKind {
    #[default]
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Player {
    #[default]
    Black,
    White,
}

impl Display for PieceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PieceKind::Pawn => 'P',
                PieceKind::Rook => 'R',
                PieceKind::Knight => 'N',
                PieceKind::Bishop => 'B',
                PieceKind::Queen => 'Q',
                PieceKind::King => 'K',
            }
        )
    }
}

impl Player {
    pub fn flip(&mut self) {
        let other = match self {
            Player::Black => Self::White,
            Player::White => Self::Black,
        };
        *self = other;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Piece {
    kind: PieceKind,
    color: Player,
}

impl Piece {
    pub const fn new_black(kind: PieceKind) -> Self {
        Self {
            kind,
            color: Player::Black,
        }
    }
    pub const fn new_white(kind: PieceKind) -> Self {
        Self {
            kind,
            color: Player::White,
        }
    }

    /// Returns `true` if the piece kind is [`Pawn`].
    ///
    /// [`Pawn`]: PieceKind::Pawn
    #[must_use]
    pub fn is_pawn(&self) -> bool {
        matches!(self.kind, PieceKind::Pawn)
    }

    /// Returns `true` if the piece kind is [`Rook`].
    ///
    /// [`Rook`]: PieceKind::Rook
    #[must_use]
    pub fn is_rook(&self) -> bool {
        matches!(self.kind, PieceKind::Rook)
    }

    /// Returns `true` if the piece kind is [`Knight`].
    ///
    /// [`Knight`]: PieceKind::Knight
    #[must_use]
    pub fn is_knight(&self) -> bool {
        matches!(self.kind, PieceKind::Knight)
    }

    /// Returns `true` if the piece kind is [`Bishop`].
    ///
    /// [`Bishop`]: PieceKind::Bishop
    #[must_use]
    pub fn is_bishop(&self) -> bool {
        matches!(self.kind, PieceKind::Bishop)
    }

    /// Returns `true` if the piece kind is [`Queen`].
    ///
    /// [`Queen`]: PieceKind::Queen
    #[must_use]
    pub fn is_queen(&self) -> bool {
        matches!(self.kind, PieceKind::Queen)
    }

    /// Returns `true` if the piece kind is [`King`].
    ///
    /// [`King`]: PieceKind::King
    #[must_use]
    pub fn is_king(&self) -> bool {
        matches!(self.kind, PieceKind::King)
    }
}

impl Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.color, self.kind) {
            (Player::Black, PieceKind::Pawn) => write!(f, "p")?,
            (Player::Black, PieceKind::Rook) => write!(f, "r")?,
            (Player::Black, PieceKind::Knight) => write!(f, "n")?,
            (Player::Black, PieceKind::Bishop) => write!(f, "b")?,
            (Player::Black, PieceKind::Queen) => write!(f, "q")?,
            (Player::Black, PieceKind::King) => write!(f, "k")?,
            (Player::White, PieceKind::Pawn) => write!(f, "P")?,
            (Player::White, PieceKind::Rook) => write!(f, "R")?,
            (Player::White, PieceKind::Knight) => write!(f, "N")?,
            (Player::White, PieceKind::Bishop) => write!(f, "B")?,
            (Player::White, PieceKind::Queen) => write!(f, "Q")?,
            (Player::White, PieceKind::King) => write!(f, "K")?,
        }

        Ok(())
    }
}
