use std::fmt::Display;

use crate::Player;

const IS_WHITE: u8 = 0b1000;
const KIND: u8 = 0b0111;

/// Of which kind a piece is
#[repr(u8)]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum PieceKind {
    /// a pawn (♙)
    #[default]
    Pawn = 1,
    /// a rook (♖)
    Rook = 2,
    /// a knight (♘)
    Knight = 3,
    /// a bishop (♗)
    Bishop = 4,
    /// a queen (♕)
    Queen = 5,
    /// a king (♔)
    King = 6,
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

/// A piece with a color. Construct this with [`Piece::new_black`] and [`Piece::new_white`]. There
/// usually is no need to construct though, as this is done by [`Game::new`]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub enum Piece {
    /// a black pawn (♙)
    #[default]
    BlackPawn = 1,
    /// a black rook (♖)
    BlackRook = 2,
    /// a black knight (♘)
    BlackKnight = 3,
    /// a black bishop (♗)
    BlackBishop = 4,
    /// a black queen (♕)
    BlackQueen = 5,
    /// a black king (♔)
    BlackKing = 6,

    /// a white pawn (♙)
    WhitePawn = 9,
    /// a white rook (♖)
    WhiteRook = 10,
    /// a white knight (♘)
    WhiteKnight = 11,
    /// a white bishop (♗)
    WhiteBishop = 12,
    /// a white queen (♕)
    WhiteQueen = 13,
    /// a white king (♔)
    WhiteKing = 14,
}

impl Piece {
    /// Constructs a new Piece with the given kind for the given player
    #[inline]
    pub const fn new(kind: PieceKind, player: Player) -> Self {
        match player {
            Player::Black => Self::new_black(kind),
            Player::White => Self::new_white(kind),
        }
    }

    /// Constructs a new dark colored piece
    #[inline]
    pub const fn new_black(kind: PieceKind) -> Self {
        match kind {
            PieceKind::Pawn => Self::BlackPawn,
            PieceKind::Rook => Self::BlackRook,
            PieceKind::Knight => Self::BlackKnight,
            PieceKind::Bishop => Self::BlackBishop,
            PieceKind::Queen => Self::BlackQueen,
            PieceKind::King => Self::BlackKing,
        }
    }

    /// Constructs a new light colored piece
    pub const fn new_white(kind: PieceKind) -> Self {
        match kind {
            PieceKind::Pawn => Self::WhitePawn,
            PieceKind::Rook => Self::WhiteRook,
            PieceKind::Knight => Self::WhiteKnight,
            PieceKind::Bishop => Self::WhiteBishop,
            PieceKind::Queen => Self::WhiteQueen,
            PieceKind::King => Self::WhiteKing,
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
        matches!(self.kind(), PieceKind::Pawn)
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
        matches!(self.kind(), PieceKind::Rook)
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
        matches!(self.kind(), PieceKind::Knight)
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
        matches!(self.kind(), PieceKind::Bishop)
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
        matches!(self.kind(), PieceKind::Queen)
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
        matches!(self.kind(), PieceKind::King)
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
        let factor = match self.player() {
            Player::Black => -1,
            Player::White => 1,
        };
        let value = match self.kind() {
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
        if (*self as u8) & IS_WHITE == 0 {
            Player::Black
        } else {
            Player::White
        }
    }

    /// returns the [`PieceKind`] of the piece, i.e. 'erases' the color
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen(getter))]
    #[inline]
    pub fn kind(&self) -> PieceKind {
        use PieceKind::*;
        match (*self as u8) & KIND {
            1 => Pawn,
            2 => Rook,
            3 => Knight,
            4 => Bishop,
            5 => Queen,
            6 => King,
            _ => unreachable!(),
        }
    }
}

impl Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.player(), self.kind()) {
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

    #[test]
    fn constructs_correctly() {
        use PieceKind::*;
        let cases = [Pawn, Rook, Knight, Bishop, Queen, King];

        for kind in cases {
            let constructed = Piece::new_black(kind);
            assert_eq!(constructed.kind(), kind);
            assert_eq!(constructed.player(), Player::Black);

            let constructed = Piece::new_white(kind);
            assert_eq!(constructed.kind(), kind);
            assert_eq!(constructed.player(), Player::White);
        }
    }
}
