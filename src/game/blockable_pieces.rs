use std::collections::VecDeque;

use crate::{happy_try, Game, Piece, Player, Position};
use directions::{EightWayDirection, FourWayDirection, Offset};

mod directions {
    #[macro_export]
    macro_rules! happy_try {
        ($x:expr) => {
            if let Some(tmp) = $x {
                return Some(tmp);
            }
        };
    }

    pub(super) trait Offset {
        #[must_use]
        fn offset(self, distance: u8) -> (i8, i8);
    }

    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
    pub(super) enum FourWayDirection {
        #[default]
        Top,
        Right,
        Bottom,
        Left,
    }

    impl Offset for FourWayDirection {
        #[must_use]
        fn offset(self, distance: u8) -> (i8, i8) {
            use FourWayDirection::*;
            let distance = distance as i8;
            match self {
                Top => (0, distance),
                Right => (distance, 0),
                Bottom => (0, -distance),
                Left => (-distance, 0),
            }
        }
    }

    impl FourWayDirection {
        #[must_use]
        pub(super) fn offset_diagonal(self, distance: u8) -> (i8, i8) {
            use FourWayDirection::*;
            let distance = distance as i8;
            match self {
                Top => (distance, distance),
                Right => (distance, -distance),
                Bottom => (-distance, distance),
                Left => (-distance, -distance),
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(super) enum EightWayDirection {
        Top,
        TopRight,
        Right,
        BottomRight,
        Bottom,
        BottomLeft,
        Left,
        TopLeft,
    }

    impl Offset for EightWayDirection {
        #[must_use]
        fn offset(self, distance: u8) -> (i8, i8) {
            use EightWayDirection::*;
            let distance = distance as i8;
            match self {
                Top => (0, distance),
                Right => (distance, 0),
                Bottom => (0, -distance),
                Left => (-distance, 0),
                TopRight => (distance, distance),
                BottomRight => (distance, -distance),
                BottomLeft => (-distance, -distance),
                TopLeft => (-distance, distance),
            }
        }
    }

    impl EightWayDirection {
        #[must_use]
        pub(super) fn rotate_one(self) -> Self {
            use EightWayDirection::*;
            match self {
                Top => TopRight,
                TopRight => Right,
                Right => BottomRight,
                BottomRight => Bottom,
                Bottom => BottomLeft,
                BottomLeft => Left,
                Left => TopLeft,
                TopLeft => Top,
            }
        }
    }
}

pub trait Mover<'a>: Iterator<Item = Position> + Sized {
    /// creates a new piece with the information at the given position.
    ///
    /// # Panics
    ///
    /// Panics if there is no piece on this square
    fn new(pos: Position, game: &'a Game) -> Self {
        let color = game.board[pos].unwrap().color;
        Self::new_with_color(pos, game, color)
    }

    /// Creates a new piece with the given color. The piece does not have to exist on the board for
    /// this function to work.
    ///
    /// # Panics
    ///
    /// This method should never panic.
    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self;
}

trait QueueExt<T> {
    fn enqueue(&mut self, val: T);
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

impl<T> QueueExt<T> for VecDeque<T> {
    fn enqueue(&mut self, val: T) {
        self.push_back(val);
    }

    fn dequeue(&mut self) -> Option<T> {
        self.pop_front()
    }

    fn peek(&self) -> Option<&T> {
        self.get(0)
    }
}

#[derive(Debug)]
struct Rotator<T, const N: usize> {
    // None indicates end of rotation
    // directions: ConstGenericRingBuffer<Option<T>, N>,
    directions: VecDeque<Option<T>>,
    distance: u8,
}

impl<T, const N: usize> Rotator<T, N>
where
    T: PartialEq + Copy + Offset + std::fmt::Debug,
{
    fn new(directions: VecDeque<Option<T>>) -> Self {
        Self {
            directions,
            distance: 1,
        }
    }

    fn shift(&mut self) {
        if let Some(top) = self.directions.dequeue() {
            if top.is_none() {
                self.distance += 1;
            }
            self.directions.enqueue(top);
        }
    }

    fn rotate(&mut self) -> Option<T> {
        // check if only marker exists
        if self.directions.len() == 1 {
            self.directions.clear();
            return None;
        }

        self.shift();

        let top = *self.directions.peek()?;
        happy_try!(top);

        // it was a delimiter
        self.shift();

        let top = *self.directions.peek()?;
        happy_try!(top);

        dbg!(&self);
        unreachable!("should not have two consecutive markers")
    }

    fn peek(&mut self) -> Option<T> {
        if let Some(first) = self.directions.peek()? {
            return Some(*first);
        }

        self.shift();

        *self.directions.peek().expect("at least one element")
    }

    #[allow(unused)]
    fn remove(&mut self, key: T) {
        for _ in 0..self.directions.len() {
            let top = self.directions.dequeue().unwrap();
            if top == Some(key) {
                continue;
            }
            self.directions.enqueue(top);
        }
    }

    fn remove_head(&mut self) {
        if let Some(None) = self.directions.dequeue() {
            self.directions.enqueue(None);
            self.directions.dequeue();
        }
    }

    fn next(&mut self, pos: Position, game: &Game, own_color: Player) -> Option<Position> {
        self.next_with(pos, game, own_color, <T as Offset>::offset)
    }

    fn next_with(
        &mut self,
        pos: Position,
        game: &Game,
        own_color: Player,
        f: impl Fn(T, u8) -> (i8, i8),
    ) -> Option<Position> {
        for _ in 0..(N + 1) {
            let direction = self.peek()?;
            let offset = f(direction, self.distance);
            let Some(pos) = pos + offset else {
                self.remove_head();
                continue;
            };
            match game.board[pos] {
                Some(Piece { color, .. }) if color == own_color => {
                    self.remove_head();
                    continue;
                }
                Some(_) => {
                    self.remove_head();
                    return Some(pos);
                }
                None => {
                    self.rotate();
                    return Some(pos);
                }
            }
        }

        None
    }
}

#[derive(Debug)]
struct FourWayRotator {
    inner: Rotator<FourWayDirection, 5>,
}

impl FourWayRotator {
    fn new() -> Self {
        use FourWayDirection::*;
        let mut directions = VecDeque::new();
        directions.enqueue(Some(Top));
        directions.enqueue(Some(Right));
        directions.enqueue(Some(Bottom));
        directions.enqueue(Some(Left));
        directions.enqueue(None);
        Self {
            inner: Rotator::new(directions),
        }
    }

    fn next(&mut self, pos: Position, game: &Game, own_color: Player) -> Option<Position> {
        self.inner.next(pos, game, own_color)
    }

    fn next_with(
        &mut self,
        pos: Position,
        game: &Game,
        own_color: Player,
        f: impl Fn(FourWayDirection, u8) -> (i8, i8),
    ) -> Option<Position> {
        self.inner.next_with(pos, game, own_color, f)
    }
}

#[derive(Debug)]
struct EightWayRotator {
    inner: Rotator<EightWayDirection, 9>,
}

impl EightWayRotator {
    fn new() -> Self {
        use EightWayDirection::*;
        let mut directions = VecDeque::new();
        directions.enqueue(Some(Top));
        directions.enqueue(Some(TopRight));
        directions.enqueue(Some(Right));
        directions.enqueue(Some(BottomRight));
        directions.enqueue(Some(Bottom));
        directions.enqueue(Some(BottomLeft));
        directions.enqueue(Some(Left));
        directions.enqueue(Some(TopLeft));
        directions.enqueue(None);
        Self {
            inner: Rotator::new(directions),
        }
    }

    fn next(&mut self, pos: Position, game: &Game, own_color: Player) -> Option<Position> {
        self.inner.next(pos, game, own_color)
    }
}

#[derive(Debug)]
pub struct RookMove<'a> {
    pos: Position,
    game: &'a Game,
    rotator: FourWayRotator,
    color: Player,
}

impl<'a> Mover<'a> for RookMove<'a> {
    fn new(pos: Position, game: &'a Game) -> Self {
        let color = game.board[pos].unwrap().color;
        Self {
            pos,
            game,
            color,
            rotator: FourWayRotator::new(),
        }
    }

    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self {
        Self {
            pos,
            game,
            color,
            rotator: FourWayRotator::new(),
        }
    }
}

impl<'a> Iterator for RookMove<'a> {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        self.rotator.next(self.pos, self.game, self.color)
    }
}

#[derive(Debug)]
pub struct BishopMove<'a> {
    pos: Position,
    game: &'a Game,
    rotator: FourWayRotator,
    color: Player,
}

impl<'a> Mover<'a> for BishopMove<'a> {
    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self {
        Self {
            pos,
            game,
            color,
            rotator: FourWayRotator::new(),
        }
    }
}

impl<'a> Iterator for BishopMove<'a> {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        self.rotator.next_with(
            self.pos,
            self.game,
            self.color,
            FourWayDirection::offset_diagonal,
        )
    }
}

/// generates the possible knight moves from `starting_pos`, on an empty board
#[derive(Debug, Clone)]
pub struct KnightMove<'a> {
    rotation: Option<EightWayDirection>,
    pos: Position,
    game: &'a Game,
    color: Player,
}

impl<'a> Mover<'a> for KnightMove<'a> {
    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self {
        Self {
            pos,
            color,
            game,
            rotation: Some(EightWayDirection::Top),
        }
    }
}

impl<'a> KnightMove<'a> {
    fn offset(dir: EightWayDirection) -> (i8, i8) {
        use EightWayDirection::*;
        match dir {
            Top => (-1, 2),
            TopRight => (1, 2),
            Right => (2, 1),
            BottomRight => (-2, 1),
            Bottom => (-1, -2),
            BottomLeft => (1, -2),
            Left => (2, -1),
            TopLeft => (-2, -1),
        }
    }
}

impl<'a> Iterator for KnightMove<'a> {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        for _ in 0..8 {
            let rot = self.rotation?;
            let new_rot = rot.rotate_one();

            if new_rot == EightWayDirection::Top {
                self.rotation = None;
            } else {
                self.rotation = Some(new_rot);
            }

            let Some(offset) = self.pos + Self::offset(rot) else {continue};
            match self.game.board[offset] {
                Some(Piece { color, .. }) if color == self.color => {
                    continue;
                }
                Some(_) | None => return Some(offset),
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct PawnMove<'a> {
    pos: Position,
    choices: VecDeque<PawnDir>,
    game: &'a Game,
    color: Player,
    is_blocked: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PawnDir {
    PushOne,
    PushTwo,
    CaptureLeft,
    CaptureRight,
}

impl PawnDir {
    fn requires_capture(&self) -> bool {
        match self {
            PawnDir::PushOne | PawnDir::PushTwo => false,
            PawnDir::CaptureLeft | PawnDir::CaptureRight => true,
        }
    }
}

impl<'a> Mover<'a> for PawnMove<'a> {
    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self {
        let mut choices = VecDeque::new();
        choices.enqueue(PawnDir::PushOne);
        choices.enqueue(PawnDir::CaptureLeft);
        choices.enqueue(PawnDir::CaptureRight);
        match color {
            Player::Black if pos.y == 6 => {
                choices.enqueue(PawnDir::PushTwo);
            }
            Player::White if pos.y == 1 => {
                choices.enqueue(PawnDir::PushTwo);
            }
            _ => {}
        }

        Self {
            pos,
            color,
            game,
            choices,
            is_blocked: false,
        }
    }

}

impl<'a> PawnMove<'a> {
    fn offset(&self, pawn_dir: PawnDir) -> (i8, i8) {
        use PawnDir::*;
        let direction = if self.color == Player::White { 1 } else { -1 };

        match pawn_dir {
            PushOne => (0, direction),
            PushTwo => (0, 2 * direction),
            CaptureLeft => (direction, direction),
            CaptureRight => (-direction, direction),
        }
    }
}

impl<'a> Iterator for PawnMove<'a> {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pawn_dir = self.choices.dequeue()?;
            if matches!(pawn_dir, PawnDir::PushOne) {
                self.is_blocked = true;
            }

            if matches!(pawn_dir, PawnDir::PushTwo) && self.is_blocked {
                continue;
            }

            let offset = self.offset(pawn_dir);
            let Some(pos) = self.pos + offset else {
                continue;
            };
            match self.game.board[pos] {
                Some(piece) if piece.color == self.color => continue,

                Some(_) if pawn_dir.requires_capture() => {
                    return Some(pos);
                }

                None if pawn_dir.requires_capture() => {
                    continue;
                }

                Some(_) => {
                    continue;
                }

                None => {
                    self.is_blocked = false;
                    return Some(pos);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct QueenMove<'a> {
    pos: Position,
    game: &'a Game,
    rotator: EightWayRotator,
    color: Player,
}

impl<'a> Mover<'a> for QueenMove<'a> {
    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self {
        Self {
            pos,
            game,
            color,
            rotator: EightWayRotator::new(),
        }
    }
}

impl<'a> Iterator for QueenMove<'a> {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        self.rotator.next(self.pos, self.game, self.color)
    }
}

#[derive(Debug)]
pub enum PieceMove<'a> {
    Pawn(PawnMove<'a>),
    Rook(RookMove<'a>),
    Knight(KnightMove<'a>),
    Bishop(BishopMove<'a>),
    Queen(QueenMove<'a>),
    King(()),
}

impl<'a> Mover<'a> for PieceMove<'a> {
    #[inline]
    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self {
        match game.board[pos].unwrap().kind {
            crate::PieceKind::Pawn => Self::Pawn(PawnMove::new_with_color(pos, game, color)),
            crate::PieceKind::Rook => Self::Rook(RookMove::new_with_color(pos, game, color)),
            crate::PieceKind::Knight => Self::Knight(KnightMove::new_with_color(pos, game, color)),
            crate::PieceKind::Bishop => Self::Bishop(BishopMove::new_with_color(pos, game, color)),
            crate::PieceKind::Queen => Self::Queen(QueenMove::new_with_color(pos, game, color)),
            crate::PieceKind::King => Self::King(()),
        }
    }
}

impl<'a> Iterator for PieceMove<'a> {
    type Item = Position;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            PieceMove::Pawn(inner) => inner.next(),
            PieceMove::Rook(inner) => inner.next(),
            PieceMove::Knight(inner) => inner.next(),
            PieceMove::Bishop(inner) => inner.next(),
            PieceMove::Queen(inner) => inner.next(),
            PieceMove::King(_inner) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use pretty_assertions::assert_eq;
    const PASSIVE_AGGRESSIVE_NOTE: &str = "i got this from chess.com, it better be valid";
    use super::*;

    macro_rules! testcase {
        ($who:ident, $name:ident, $fen:literal, $start:literal => $($expected:literal),*) => {
            #[test]
            fn $name() {
                let game = Game::from_fen($fen).expect(PASSIVE_AGGRESSIVE_NOTE);
                let mut moves = $who::new($start.parse().unwrap(), &game)
                    .map(|x| x.to_string())
                    .collect_vec();
                let mut expected = [$($expected,)*]
                    .map(|x: &str| x.parse::<Position>().unwrap().to_string());
                moves.sort();
                expected.sort();

                assert_eq!(moves, &expected);
            }
        };
    }

    #[test]
    fn rook_cannot_move() {
        let game = Game::new();
        let moves = RookMove::new(Position::new(0, 0), &game).collect_vec();
        assert_eq!(moves, &[]);
    }

    testcase!(KnightMove, knight_move, "rn2kb1r/p1q1pp1p/2p2np1/1p6/3PB3/2N2N2/PPP2PPP/R1BQ1RK1 w kq - 2 10", "c3"
        => "b5", "a4", "b1", "e2", "d5" );

    testcase!(BishopMove, bishop_move, "rn2kb1r/p1q1pp1p/2p2np1/1p6/3PB3/2N2N2/PPP2PPP/R1BQ1RK1 w kq - 2 10", "e4"
        => "d3", "f5", "g6", "d5", "c6" );

    testcase!(RookMove, rook_move, "2r2r2/p3Rp1p/nnkp2p1/8/P1P5/5N2/1P3PPP/R2Q2K1 w - - 1 22", "e7"
        => "f7", "e8", "e6", "e5", "e4", "e3", "e2", "e1", "d7", "c7", "b7", "a7");

    testcase!(QueenMove, queen_move, "2r2r2/p3Rp1p/nnkp2p1/8/P1P5/5N2/1P3PPP/R2Q2K1 w - - 1 22", "d1"
        => "c1", "b1", "e1", "f1", "c2", "d2", "e2", "b3", "d3", "d4", "d5", "d6");

    testcase!(PawnMove, pawn_just_push, "2r2r2/p3Rp1p/nnkp2p1/8/P1P5/5N2/1P3PPP/R2Q2K1 w - - 1 22", "c4"
        => "c5");

    testcase!(PawnMove, pawn_double_push, "2r2r2/p3Rp1p/nnkp2p1/8/P1P5/5N2/1P3PPP/R2Q2K1 w - - 1 22", "g2"
        => "g3", "g4");

    testcase!(PawnMove, pawn_blocked, "2r2r2/p3Rp1p/nnkp2p1/8/P1P5/5N2/1P3PPP/R2Q2K1 w - - 1 22", "f2"
        => );

    testcase!(PawnMove, pawn_capture, "2r2r2/p3Rp1p/nnk3p1/2Pp4/P7/5N2/1P3PPP/R2Q2K1 w - - 0 23", "c5"
        => "b6");
}
