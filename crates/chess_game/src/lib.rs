#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

pub mod game;
mod ply;
mod position;
mod piece;
pub use piece::{ Piece, PieceKind };
pub use position::Position;

use std::fmt::Display;

pub use game::Game;
pub use ply::{ParsePureError, ParseSanError, Ply};

/// Represents the current player.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
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
