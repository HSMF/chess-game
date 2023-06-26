use super::Eval;
use chess_game::{Game, Piece, PieceKind, Player, Position};

trait Value {
    /// determines the value of Self in centipawns
    fn value(&self) -> i64;
}

impl Value for PieceKind {
    #[inline]
    fn value(&self) -> i64 {
        // from wikipedia
        match self {
            PieceKind::Pawn => 100,   //100
            PieceKind::Knight => 326, //315
            PieceKind::Bishop => 337, //341
            PieceKind::Rook => 525,   // 500
            PieceKind::Queen => 973,  // 950
            PieceKind::King => 0,
        }
    }
}
impl Value for Piece {
    #[inline]
    fn value(&self) -> i64 {
        self.kind().value()
    }
}

/// counts the number of pieces of the board, doesn't count kings because that would be silly
#[derive(Debug, Default)]
struct PieceCountExact {
    pawns: u8,
    rooks: u8,
    knights: u8,
    bishops: u8,
    queens: u8,
}

impl PieceCountExact {
    #[inline]
    fn count(&mut self, piece: Piece) {
        match piece.kind() {
            PieceKind::Pawn => self.pawns += 1,
            PieceKind::Rook => self.rooks += 1,
            PieceKind::Knight => self.knights += 1,
            PieceKind::Bishop => self.bishops += 1,
            PieceKind::Queen => self.queens += 1,
            _ => {}
        }
    }

    fn total(&self) -> i64 {
        (self.pawns + self.rooks + self.knights + self.bishops + self.queens) as i64
    }

    fn total_value(&self) -> i64 {
        self.pawns as i64
            + (self.rooks as i64 * PieceKind::Rook.value())
            + (self.knights as i64 * PieceKind::Knight.value())
            + (self.bishops as i64 * PieceKind::Bishop.value())
            + (self.queens as i64 * PieceKind::Queen.value())
    }
}

#[derive(Debug, Default)]
struct PieceCounts {
    black: PieceCountExact,
    white: PieceCountExact,
}

impl PieceCounts {
    fn count(&mut self, piece: Piece) {
        match piece.player() {
            Player::Black => self.black.count(piece),
            Player::White => self.white.count(piece),
        }
    }

    fn total_abs(&self) -> i64 {
        self.white.total() + self.black.total()
    }

    fn total(&self) -> i64 {
        self.white.total() - self.black.total()
    }
}

#[inline]
fn position_penalty(piece: Piece, pos: Position) -> i64 {
    let y = match piece.player() {
        Player::Black => pos.y(),
        Player::White => 7 - pos.y(),
    };
    let idx = (pos.x() + y * 8) as usize;
    let table = match piece.kind() {
        PieceKind::Pawn => {
            // only for middle game atm
            #[rustfmt::skip]
            let table: [i8; 64] = [
                 0,    0,      0,  0,      0,      0,      0,      0,
                 0,    0,      0,  0,      0,      0,      0,      0 ,
                 10,   10,    12,  11,      11,      12,      10,      10,
                 15,   15,    17,  20,      20,      17,      15,      15,
                 20,   20,    22,  25,      25,      22,      20,      20,
                 10,   10,    15,  10,      10,      15,      10,      10,
                -10,  -10,   -15,  -15,     -15,     -15,     -10,     -10,
                -100, -100, -100,  -100,     -100,     -100,     -100,     -100,
            ];

            table
        }
        PieceKind::Rook => [0; 64],
        PieceKind::Knight => {
            #[rustfmt::skip]
            let table = [
                -20, -16, -12, -12, -12, -12, -16, -20,
                -8,   -4,   0,   0,   0,   0,  -4,  -8,
                -12,   4,  12,  12,  12,  12,   4, -12,
                -12,   2,   6,  10,  10,   6,   2, -12,
                -12,   2,   6,  10,  10,   6,   2, -12,
                -6,   10,   8,   6,   6,   8,   2,  -6,
                -16,  -8,   0,   2,   2,   0,  -8, -16,
                -24, -40, -12, -12, -12, -12, -40, -24,
            ];

            table
        }
        PieceKind::Bishop => [0; 64],
        PieceKind::Queen => [0; 64],
        PieceKind::King => {
            #[rustfmt::skip]
            let table = [
                -20, -20, -20, -20, -20, -20, -20, -20,
                -20, -20, -20, -20, -20, -20, -20, -20 ,
                -20, -20, -20, -20, -20, -20, -20, -20,
                -20, -20, -20, -20, -20, -20, -20, -20,
                -20, -20, -20, -20, -20, -20, -20, -20,
                -20, -20, -20, -20, -20, -20, -20, -20,
                -15, -15, -15, -15, -15, -15, -15, -15,
                20, 27, 16, -6, -3, 16, 22, 20,
            ];

            table
        }
    };
    table[idx] as i64
}

/// gives an evaluation for the Position
pub fn evaluate(game: &Game) -> Eval {
    let mut score_white = 0;
    let mut score_black = 0;

    let mut counts = PieceCounts::default();

    let mut pawns_files = [0; 8];

    for (pos, piece) in game.pieces() {
        let mut score = 0;
        counts.count(piece);

        if piece.is_pawn() {
            pawns_files[pos.x() as usize] += 1;
        }

        score += position_penalty(piece, pos);

        match piece.player() {
            Player::Black => score_black += score,
            Player::White => score_white += score,
        };
    }
    score_black += counts.black.total_value();
    score_white += counts.white.total_value();

    if counts.black.bishops >= 2 {
        score_black += 50;
    }
    if counts.white.bishops >= 2 {
        score_white += 50;
    }

    let num_pawns = counts.white.pawns + counts.black.pawns;
    // num pawns is always ≤ 16

    let adjust_knight = (num_pawns as i64 * 25 / 16) - 5;
    let adjust_sliding = (16 - num_pawns) as i64 * 7 / 16;
    score_white += adjust_knight * (counts.white.knights as i64);
    score_black += adjust_knight * (counts.black.knights as i64);

    score_white += adjust_sliding * (counts.white.rooks as i64);
    score_black += adjust_sliding * (counts.black.rooks as i64);

    score_white += adjust_sliding * (counts.white.queens as i64);
    score_black += adjust_sliding * (counts.black.queens as i64);

    score_white += adjust_sliding * (counts.white.bishops as i64);
    score_black += adjust_sliding * (counts.black.bishops as i64);

    // TODO: Knights vs Bishops
    // -> determine whether it is an open or closed position
    //
    // if counts.total() >= 30_000 {
    //     // middlegame
    // } else {
    //     // endgame
    // }

    Eval::Advantage(score_white - score_black)
}
