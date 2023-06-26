#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

pub mod game;
mod ply;
mod position;
pub use position::Position;

use std::fmt::Display;

pub use game::Game;
pub use ply::{ParsePureError, ParseSanError, Ply};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

/// Of which kind a piece is
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum PieceKind {
    /// a pawn (♙)
    #[default]
    Pawn,
    /// a rook (♖)
    Rook,
    /// a knight (♘)
    Knight,
    /// a bishop (♗)
    Bishop,
    /// a queen (♕)
    Queen,
    /// a king (♔)
    King,
}

/// Represents the current player.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub enum Player {
    /// The player playing the dark colored pieces
    #[default]
    Black,
    /// The player playing the light colored pieces (the ones that start)
    White,
}

impl Display for Player {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Player::Black => 'b',
                Player::White => 'w',
            }
        )
    }
}

impl Display for PieceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PieceKind::Pawn => 'p',
                PieceKind::Rook => 'r',
                PieceKind::Knight => 'n',
                PieceKind::Bishop => 'b',
                PieceKind::Queen => 'q',
                PieceKind::King => 'k',
            }
        )
    }
}

impl Player {
    /// changes the inner value to the player that wasn't playing
    #[inline]
    pub fn flip(&mut self) {
        let other = self.other();
        *self = other;
    }

    /// returns whose turn it isn't
    #[inline]
    pub const fn other(&self) -> Self {
        match self {
            Player::Black => Player::White,
            Player::White => Player::Black,
        }
    }

    /// returns the rank on which the pawns of this player may promote
    #[inline]
    pub const fn promotion_rank(&self) -> u8 {
        match self {
            Player::Black => 0,
            Player::White => 7,
        }
    }

    /// returns the rank on which the pieces of this player start out
    #[inline]
    pub const fn home_rank(&self) -> u8 {
        match self {
            Player::Black => 7,
            Player::White => 0,
        }
    }

    /// returns the rank on which the pawns of this player start out
    #[inline]
    pub const fn pawn_rank(&self) -> u8 {
        match self {
            Player::Black => 6,
            Player::White => 1,
        }
    }
}

/// A piece with a color. Construct this with [`Piece::new_black`] and [`Piece::new_white`]. There
/// usually is no need to construct though, as this is done by [`Game::new`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub struct Piece {
    kind: PieceKind,
    color: Player,
}

impl Piece {
    /// Constructs a new dark colored piece
    pub const fn new_black(kind: PieceKind) -> Self {
        Self {
            kind,
            color: Player::Black,
        }
    }

    /// Constructs a new light colored piece
    pub const fn new_white(kind: PieceKind) -> Self {
        Self {
            kind,
            color: Player::White,
        }
    }

    /// Returns `true` if the piece kind is [`Pawn`].
    ///
    /// [`Pawn`]: PieceKind::Pawn
    ///
    /// ```
    /// # use chess_game::*;
    /// assert_eq!(Piece::new_black(PieceKind::Pawn).is_pawn(), true);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_pawn(&self) -> bool {
        matches!(self.kind, PieceKind::Pawn)
    }

    /// Returns `true` if the piece kind is [`Rook`].
    ///
    /// [`Rook`]: PieceKind::Rook
    ///
    /// ```
    /// # use chess_game::*;
    /// assert_eq!(Piece::new_black(PieceKind::Rook).is_rook(), true);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_rook(&self) -> bool {
        matches!(self.kind, PieceKind::Rook)
    }

    /// Returns `true` if the piece kind is [`Knight`].
    ///
    /// [`Knight`]: PieceKind::Knight
    ///
    /// ```
    /// # use chess_game::*;
    /// assert_eq!(Piece::new_black(PieceKind::Knight).is_knight(), true);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_knight(&self) -> bool {
        matches!(self.kind, PieceKind::Knight)
    }

    /// Returns `true` if the piece kind is [`Bishop`].
    ///
    /// [`Bishop`]: PieceKind::Bishop
    ///
    /// ```
    /// # use chess_game::*;
    /// assert_eq!(Piece::new_white(PieceKind::Bishop).is_bishop(), true);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_bishop(&self) -> bool {
        matches!(self.kind, PieceKind::Bishop)
    }

    /// Returns `true` if the piece kind is [`Queen`].
    ///
    /// [`Queen`]: PieceKind::Queen
    ///
    /// ```
    /// # use chess_game::*;
    /// assert_eq!(Piece::new_black(PieceKind::Queen).is_queen(), true);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_queen(&self) -> bool {
        matches!(self.kind, PieceKind::Queen)
    }

    /// Returns `true` if the piece kind is [`King`].
    ///
    /// [`King`]: PieceKind::King
    ///
    /// ```
    /// # use chess_game::*;
    /// assert_eq!(Piece::new_white(PieceKind::King).is_king(), true);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_king(&self) -> bool {
        matches!(self.kind, PieceKind::King)
    }

    /// Returns the piece valuation, relative to pawns
    ///
    /// ```
    /// # use chess_game::*;
    ///
    /// assert_eq!(Piece::new_black(PieceKind::Queen).value(), -9);
    /// assert_eq!(Piece::new_black(PieceKind::King).value()
    ///          + Piece::new_white(PieceKind::King).value(), 0);
    /// ```
    #[inline]
    pub fn value(&self) -> i64 {
        let factor = match self.color {
            Player::Black => -1,
            Player::White => 1,
        };
        let value = match self.kind {
            PieceKind::Pawn => 1,
            PieceKind::Rook => 5,
            PieceKind::Knight => 3,
            PieceKind::Bishop => 3,
            PieceKind::Queen => 9,
            // king is ±1000 and not i64::MAX/MIN so that adding the kings cancels out
            PieceKind::King => 1000,
        };
        value * factor
    }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
impl Piece {
    /// returns to which player the piece belongs
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen(getter))]
    #[inline]
    pub fn player(&self) -> Player {
        self.color
    }

    /// returns the [`PieceKind`] of the piece, i.e. 'erases' the color
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen(getter))]
    #[inline]
    pub fn kind(&self) -> PieceKind {
        self.kind
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn display_piece_kind() {
        assert_eq!(PieceKind::Pawn.to_string(), "p");
        assert_eq!(PieceKind::Rook.to_string(), "r");
        assert_eq!(PieceKind::Knight.to_string(), "n");
        assert_eq!(PieceKind::Bishop.to_string(), "b");
        assert_eq!(PieceKind::Queen.to_string(), "q");
        assert_eq!(PieceKind::King.to_string(), "k");
    }
}
