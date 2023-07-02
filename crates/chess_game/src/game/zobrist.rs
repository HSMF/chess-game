//! module for [Zobrist Hashing](https://en.wikipedia.org/wiki/Zobrist_hashing)

use std::fmt::Display;

use crate::{Game, Piece, PieceKind, Player, Ply, Position};
mod values;

use values::BLACK_TO_MOVE;

use self::values::{
    BLACK_BISHOP, BLACK_KING, BLACK_KNIGHT, BLACK_PAWN, BLACK_QUEEN, BLACK_ROOK,
    CASTLING_RIGHT_ALL_ALL, CASTLING_RIGHT_ALL_K, CASTLING_RIGHT_ALL_NONE, CASTLING_RIGHT_ALL_Q,
    CASTLING_RIGHT_K_ALL, CASTLING_RIGHT_K_K, CASTLING_RIGHT_K_NONE, CASTLING_RIGHT_K_Q,
    CASTLING_RIGHT_NONE_ALL, CASTLING_RIGHT_NONE_K, CASTLING_RIGHT_NONE_NONE,
    CASTLING_RIGHT_NONE_Q, CASTLING_RIGHT_Q_ALL, CASTLING_RIGHT_Q_K, CASTLING_RIGHT_Q_NONE,
    CASTLING_RIGHT_Q_Q, EN_PASSANT_FILE, WHITE_BISHOP, WHITE_KING, WHITE_KNIGHT, WHITE_PAWN,
    WHITE_QUEEN, WHITE_ROOK,
};

use super::{CastlingRights, MoveInfo};

/// The hashed value for a board. A hash can be created
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Zobrist(u64);

impl Display for Zobrist {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", self.0)
    }
}

fn lookup_piece(piece: Piece, pos: Position) -> u64 {
    let idx = Zobrist::to_idx(pos);

    let lookup = match (piece.kind(), piece.player()) {
        (PieceKind::Pawn, Player::Black) => BLACK_PAWN,
        (PieceKind::Pawn, Player::White) => WHITE_PAWN,
        (PieceKind::Rook, Player::Black) => BLACK_ROOK,
        (PieceKind::Rook, Player::White) => WHITE_ROOK,
        (PieceKind::Knight, Player::Black) => BLACK_KNIGHT,
        (PieceKind::Knight, Player::White) => WHITE_KNIGHT,
        (PieceKind::Bishop, Player::Black) => BLACK_BISHOP,
        (PieceKind::Bishop, Player::White) => WHITE_BISHOP,
        (PieceKind::Queen, Player::Black) => BLACK_QUEEN,
        (PieceKind::Queen, Player::White) => WHITE_QUEEN,
        (PieceKind::King, Player::Black) => BLACK_KING,
        (PieceKind::King, Player::White) => WHITE_KING,
    };

    lookup[idx]
}

fn lookup_en_passant(pos: Option<Position>) -> u64 {
    if let Some(en_passant) = pos {
        EN_PASSANT_FILE[en_passant.x() as usize]
    } else {
        0
    }
}

fn lookup_castling_rights(white: CastlingRights, black: CastlingRights) -> u64 {
    match (
        white.king_side,
        white.queen_side,
        black.king_side,
        black.queen_side,
    ) {
        (true, true, true, true) => CASTLING_RIGHT_ALL_ALL,
        (true, true, true, false) => CASTLING_RIGHT_ALL_K,
        (true, true, false, true) => CASTLING_RIGHT_ALL_Q,
        (true, true, false, false) => CASTLING_RIGHT_ALL_NONE,
        (true, false, true, true) => CASTLING_RIGHT_K_ALL,
        (true, false, true, false) => CASTLING_RIGHT_K_K,
        (true, false, false, true) => CASTLING_RIGHT_K_Q,
        (true, false, false, false) => CASTLING_RIGHT_K_NONE,
        (false, true, true, true) => CASTLING_RIGHT_Q_ALL,
        (false, true, true, false) => CASTLING_RIGHT_Q_K,
        (false, true, false, true) => CASTLING_RIGHT_Q_Q,
        (false, true, false, false) => CASTLING_RIGHT_Q_NONE,
        (false, false, true, true) => CASTLING_RIGHT_NONE_ALL,
        (false, false, true, false) => CASTLING_RIGHT_NONE_K,
        (false, false, false, true) => CASTLING_RIGHT_NONE_Q,
        (false, false, false, false) => CASTLING_RIGHT_NONE_NONE,
    }
}

impl Zobrist {
    /// Returns the computed value for the hash
    pub fn get(&self) -> u64 {
        self.0
    }

    pub(super) fn new_zeroed() -> Self {
        Self(0)
    }

    /// construct a new Hash from the given game
    pub fn new(game: &Game) -> Self {
        let mut hash = 0;

        if game.player_to_move() == Player::Black {
            hash ^= BLACK_TO_MOVE;
        }

        for (pos, piece) in game.pieces() {
            hash ^= lookup_piece(piece, pos);
        }

        hash ^= lookup_en_passant(game.en_passant_sq);
        hash ^= lookup_castling_rights(game.castling_white, game.castling_black);

        Self(hash)
    }

    /// update the hash by the move info.
    ///
    /// Call this after [`Game::try_make_move`]
    ///
    /// # Panics
    /// May panic if the [MoveInfo] does not correspond to the last move played
    pub fn update(self, move_info: MoveInfo, game: &Game) -> Self {
        let Zobrist(mut hash) = self;
        let MoveInfo {
            ply,
            halfmove: _,
            captured,
            castling_rights,
            en_passant_sq,
        } = move_info;
        let player = game.to_move.other();
        hash ^= BLACK_TO_MOVE;

        hash ^= lookup_en_passant(en_passant_sq);

        match player {
            Player::Black => hash ^= lookup_castling_rights(game.castling_white, castling_rights),
            Player::White => hash ^= lookup_castling_rights(castling_rights, game.castling_black),
        }

        hash ^= lookup_castling_rights(game.castling_white, game.castling_black);
        hash ^= lookup_en_passant(game.en_passant_sq);

        match ply {
            Ply::Move {
                from,
                to,
                promoted_to,
            } => {
                let piece_that_moved = game[to].unwrap();
                // remove from origin
                hash ^= lookup_piece(piece_that_moved, from);
                // put on new square
                if let Some(promoted_to) = promoted_to {
                    hash ^= lookup_piece(Piece::new(promoted_to, player), to);
                } else {
                    hash ^= lookup_piece(piece_that_moved, to);
                }

                if let Some((captured, pos)) = captured {
                    hash ^= lookup_piece(captured, pos);
                }
            }
            Ply::Castle => {
                let home_row = player.home_rank();
                let king = Piece::new(PieceKind::King, player);
                let rook = Piece::new(PieceKind::Rook, player);
                let king_from = Position::new(4, home_row);
                let king_to = Position::new(6, home_row);
                let rook_from = Position::new(7, home_row);
                let rook_to = Position::new(5, home_row);

                hash ^= lookup_piece(king, king_from);
                hash ^= lookup_piece(king, king_to);
                hash ^= lookup_piece(rook, rook_from);
                hash ^= lookup_piece(rook, rook_to);
            }
            Ply::LongCastle => {
                let home_row = player.home_rank();
                let king = Piece::new(PieceKind::King, player);
                let rook = Piece::new(PieceKind::Rook, player);
                let king_from = Position::new(4, home_row);
                let king_to = Position::new(2, home_row);
                let rook_from = Position::new(0, home_row);
                let rook_to = Position::new(3, home_row);

                hash ^= lookup_piece(king, king_from);
                hash ^= lookup_piece(king, king_to);
                hash ^= lookup_piece(rook, rook_from);
                hash ^= lookup_piece(rook, rook_to);
            }
        }

        Zobrist(hash)
    }

    /// reverts the move.
    /// This has the same effect as [`Game::unmake_move`], only for the hash
    ///
    /// Call this before [`Game::unmake_move`]
    ///
    /// # Panics
    /// May panic if the [MoveInfo] does not correspond to the last move played
    ///
    pub fn revert(self, move_info: MoveInfo, game: &Game) -> Self {
        self.update(move_info, game)
    }

    fn to_idx(pos: Position) -> usize {
        pos.x() as usize + pos.y() as usize * 8
    }
}

#[cfg(test)]
mod tests {
    use crate::game::PieceMove;

    use super::*;

    fn all_moves(game: &Game) -> Vec<Ply> {
        let player = game.player_to_move();

        let mut out = vec![];

        for (pos, piece) in game.pieces().filter(|(_, piece)| piece.player() == player) {
            for to in PieceMove::new_with_piece(pos, game, piece) {
                if piece.is_pawn() && to.y() == player.promotion_rank() {
                    use PieceKind::*;
                    for promoted_to in [Rook, Knight, Bishop, Queen] {
                        if let Some(ply) = Ply::from_positions(pos, to, Some(promoted_to), game) {
                            out.push(ply);
                        }
                    }
                } else if let Some(ply) = Ply::from_positions(pos, to, None, game) {
                    out.push(ply);
                }
            }
        }

        out
    }

    macro_rules! test_all_moves {
        ($name:ident, $pos:literal) => {
            #[test]
            fn $name() {
                let mut game: Game = $pos.parse().unwrap();

                let before = game.clone();
                for ply in all_moves(&game) {
                    let move_info = match game.try_make_move(ply) {
                        Ok(mi) => mi,
                        Err(_) => {
                            assert_eq!(before, game);
                            continue;
                        }
                    };

                    let expected = Zobrist::new(&game);
                    let actual = before.hash.update(move_info, &game);

                    assert_eq!(actual, expected, "move was: {ply}");
                    assert_eq!(game.hash, expected, "move was: {ply}");

                    let actual = actual.revert(move_info, &game);
                    game.unmake_move(move_info);
                    assert_eq!(game, before, "move was: {ply}");

                    assert_eq!(actual, before.hash, "move was: {ply}");
                    assert_eq!(game.hash, before.hash, "move was: {ply}");
                }
            }
        };
    }

    test_all_moves!(
        hashes_correctly_no_castle,
        "r3k2r/1p1Nbppp/p1B4n/2pp4/3P4/2N5/PPP2PPP/R1BQ1RK1 b kq - 0 12"
    );

    test_all_moves!(
        hashes_correctly_castle,
        "r1b2rk1/4qpp1/p1np1n1p/1p2p1B1/2P1P3/N2B4/PP3PPP/R2QK2R w KQ - 0 14"
    );
}
