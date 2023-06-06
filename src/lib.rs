//! # Chess Game
//!
//! A simple chess implementation.
//!
//! ```
//! use chess_game::{Game, Ply, Player};
//!
//! // set up a new game
//! let mut game = Game::new();
//! // play a king's pawn opening
//! assert_eq!(game.player_to_move(), Player::White);
//! game.try_make_move(Ply::parse_san("e4", &game).expect("valid notation")).expect("valid move");
//!
//! // respond with sicilian defense
//! assert_eq!(game.player_to_move(), Player::Black);
//! game.try_make_move(Ply::parse_san("c5", &game).expect("valid notation")).expect("valid move");
//! assert_eq!(game.player_to_move(), Player::White);
//!
//! game.try_make_move(Ply::parse_san("Nf3", &game).expect("valid notation")).expect("valid move");
//! assert_eq!(game.player_to_move(), Player::Black);
//! ```

#![warn(missing_docs)]

pub mod game;
pub mod graphics;
mod ply;
mod position;
pub use position::Position;

use std::fmt::Display;

pub use game::Game;
pub use ply::{Ply, ParsePureError, ParseSanError};

/// Of which kind a piece is
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Player {
    /// The player playing the dark colored pieces
    #[default]
    Black,
    /// The player playing the light colored pieces (the ones that start)
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
    /// changes the inner value to the player that wasn't playing
    pub fn flip(&mut self) {
        let other = self.other();
        *self = other;
    }

    /// returns whose turn it isn't
    pub fn other(&self) -> Self {
        match self {
            Player::Black => Player::White,
            Player::White => Player::Black,
        }
    }
}

/// A piece with a color. Construct this with [`Piece::new_black`] and [`Piece::new_white`]. There
/// usually is no need to construct though, as this is done by [`Game::new`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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
