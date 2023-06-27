use crate::{Game, Piece, PieceKind, Player, Position};
use directions::{EightWayDirection, FourWayDirection, Offset};

use self::partial::PieceMovePartial;

mod directions {
    /// short circuits if the value was [`Some`].
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

        #[must_use]
        fn rotate_once(self) -> Option<Self>
        where
            Self: Sized;
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

        #[must_use]
        fn rotate_once(self) -> Option<Self>
        where
            Self: Sized,
        {
            match self {
                FourWayDirection::Top => Some(Self::Right),
                FourWayDirection::Right => Some(Self::Bottom),
                FourWayDirection::Bottom => Some(Self::Left),
                FourWayDirection::Left => None,
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

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub(super) enum EightWayDirection {
        #[default]
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

        #[must_use]
        fn rotate_once(self) -> Option<Self>
        where
            Self: Sized,
        {
            match self {
                EightWayDirection::Top => Some(EightWayDirection::TopRight),
                EightWayDirection::TopRight => Some(EightWayDirection::Right),
                EightWayDirection::Right => Some(EightWayDirection::BottomRight),
                EightWayDirection::BottomRight => Some(EightWayDirection::Bottom),
                EightWayDirection::Bottom => Some(EightWayDirection::BottomLeft),
                EightWayDirection::BottomLeft => Some(EightWayDirection::Left),
                EightWayDirection::Left => Some(EightWayDirection::TopLeft),
                EightWayDirection::TopLeft => None,
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

/// Combines the creation and usage of a Iterator over Piece moves
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

/// Associates the mover back to its kind and color, without needing an instance
pub trait HasPieceKind {
    /// Returns the piece kind of the mover
    fn kind() -> PieceKind;
}

#[derive(Debug, PartialEq, Eq)]
struct Rotator<T, const N: usize> {
    directions: Option<T>,
    distance: u8,
}

impl<T, const N: usize> Rotator<T, N>
where
    T: PartialEq + Copy + Offset + std::fmt::Debug + Default,
{
    fn new() -> Self {
        Self {
            directions: Some(T::default()),
            distance: 1,
        }
    }

    fn rotate_once(&mut self) {
        if let Some(direction) = self.directions {
            self.directions = direction.rotate_once();
            self.distance = 1;
        }
    }

    #[inline]
    fn next(&mut self, pos: Position, game: &Game, own_color: Player) -> Option<Position> {
        self.next_with(pos, game, own_color, <T as Offset>::offset)
    }

    #[inline]
    fn next_with(
        &mut self,
        pos: Position,
        game: &Game,
        own_color: Player,
        f: impl Fn(T, u8) -> (i8, i8),
    ) -> Option<Position> {
        for _ in 1..8 {
            let direction = self.directions?;
            let offset = f(direction, self.distance);
            let Some(pos) = pos + offset else {
                self.rotate_once();
                continue;
            };
            match game.board[pos] {
                // can't capture own piece
                Some(Piece { color, .. }) if color == own_color => {
                    self.rotate_once();
                    continue;
                }
                // can capture enemy piece
                Some(_) => {
                    self.rotate_once();
                    return Some(pos);
                }
                // can move on empty square
                None => {
                    self.distance += 1;
                    return Some(pos);
                }
            }
        }

        None
    }
}

#[derive(Debug, PartialEq, Eq)]
struct FourWayRotator {
    inner: Rotator<FourWayDirection, 8>,
}

impl FourWayRotator {
    fn new() -> Self {
        Self {
            inner: Rotator::new(),
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

#[derive(Debug, PartialEq, Eq)]
struct EightWayRotator {
    inner: Rotator<EightWayDirection, 16>,
}

impl EightWayRotator {
    fn new() -> Self {
        Self {
            inner: Rotator::new(),
        }
    }

    #[inline]
    fn next(&mut self, pos: Position, game: &Game, own_color: Player) -> Option<Position> {
        self.inner.next(pos, game, own_color)
    }
}

/// Iterator over the possible moves that the rook can make.
///
/// Construct this via the [`Mover`] trait or via [`PieceMove`]
#[derive(Debug, PartialEq, Eq)]
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

impl<'a> HasPieceKind for RookMove<'a> {
    fn kind() -> PieceKind {
        PieceKind::Rook
    }
}

impl<'a> Iterator for RookMove<'a> {
    type Item = Position;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.rotator.next(self.pos, self.game, self.color)
    }
}

/// Iterator over the possible moves that the bishop can make.
///
/// Construct this via the [`Mover`] trait or via [`PieceMove`]
#[derive(Debug, PartialEq, Eq)]
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

impl<'a> HasPieceKind for BishopMove<'a> {
    fn kind() -> PieceKind {
        PieceKind::Bishop
    }
}

impl<'a> Iterator for BishopMove<'a> {
    type Item = Position;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.rotator.next_with(
            self.pos,
            self.game,
            self.color,
            FourWayDirection::offset_diagonal,
        )
    }
}

/// Iterator over the possible moves that the knight can make.
///
/// Construct this via the [`Mover`] trait or via [`PieceMove`]
#[derive(Debug, Clone, PartialEq, Eq)]
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

impl<'a> HasPieceKind for KnightMove<'a> {
    fn kind() -> PieceKind {
        PieceKind::Knight
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

    #[inline]
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

/// Iterator over the possible moves that the pawn can make.
///
/// Construct this via the [`Mover`] trait or via [`PieceMove`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PawnMove<'a> {
    pos: Position,
    game: &'a Game,
    color: Player,
    is_blocked: bool,
    push_two: bool,
    dir: Option<PawnDir>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum PawnDir {
    #[default]
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

    #[inline]
    fn next(self) -> Option<Self> {
        match self {
            PawnDir::PushOne => Some(PawnDir::PushTwo),
            PawnDir::PushTwo => Some(PawnDir::CaptureLeft),
            PawnDir::CaptureLeft => Some(PawnDir::CaptureRight),
            PawnDir::CaptureRight => None,
        }
    }
}

impl<'a> Mover<'a> for PawnMove<'a> {
    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self {
        Self {
            pos,
            color,
            game,
            push_two: pos.y() == color.pawn_rank(),
            dir: Some(PawnDir::PushOne),
            is_blocked: false,
        }
    }
}

impl<'a> HasPieceKind for PawnMove<'a> {
    fn kind() -> PieceKind {
        PieceKind::Pawn
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

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pawn_dir = self.dir?;
            self.dir = pawn_dir.next();

            if matches!(pawn_dir, PawnDir::PushOne) {
                self.is_blocked = true;
            }

            match pawn_dir {
                PawnDir::PushTwo if !self.push_two => continue,
                PawnDir::PushTwo if self.is_blocked => continue,
                _ => (),
            }

            let offset = self.offset(pawn_dir);
            let Some(pos) = self.pos + offset else {
                continue;
            };
            if pawn_dir.requires_capture() && Some(pos) == self.game.en_passant_sq {
                return Some(pos);
            }

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

/// Iterator over the possible moves that the queen can make.
///
/// Construct this via the [`Mover`] trait or via [`PieceMove`]
#[derive(Debug, PartialEq, Eq)]
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

impl<'a> HasPieceKind for QueenMove<'a> {
    fn kind() -> PieceKind {
        PieceKind::Queen
    }
}

impl<'a> Iterator for QueenMove<'a> {
    type Item = Position;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.rotator.next(self.pos, self.game, self.color)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum KingsMoves {
    Rotating(EightWayDirection),
    Castle,
    LongCastle,
    None,
}

/// Iterator over the possible moves that the king can make.
///
/// Construct this via the [`Mover`] trait or via [`PieceMove`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KingMove<'a> {
    rotation: KingsMoves,
    pos: Position,
    game: &'a Game,
    color: Player,
}

impl<'a> Mover<'a> for KingMove<'a> {
    fn new_with_color(pos: Position, game: &'a Game, color: Player) -> Self {
        Self {
            pos,
            color,
            game,
            rotation: KingsMoves::Rotating(EightWayDirection::Top),
        }
    }
}

impl<'a> HasPieceKind for KingMove<'a> {
    fn kind() -> PieceKind {
        PieceKind::King
    }
}

impl<'a> Iterator for KingMove<'a> {
    type Item = Position;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        for _ in 0..11 {
            match self.rotation {
                KingsMoves::Rotating(rot) => {
                    let new_rot = rot.rotate_one();

                    if new_rot == EightWayDirection::Top {
                        self.rotation = KingsMoves::Castle;
                    } else {
                        self.rotation = KingsMoves::Rotating(new_rot);
                    }

                    let Some(offset) = self.pos + rot.offset(1) else {continue};
                    match self.game.board[offset] {
                        Some(Piece { color, .. }) if color == self.color => {
                            continue;
                        }
                        Some(_) | None => return Some(offset),
                    }
                }
                KingsMoves::Castle => {
                    self.rotation = KingsMoves::LongCastle;
                    if !self.game.castling_rights(self.color).king_side {
                        continue;
                    }

                    if self.game.board[Position::new(5, self.pos.y())].is_some() {
                        continue;
                    }

                    if self.game.board[Position::new(6, self.pos.y())].is_some() {
                        continue;
                    }

                    return Some(Position::new(6, self.pos.y()));
                }
                KingsMoves::LongCastle => {
                    self.rotation = KingsMoves::None;
                    if !self.game.castling_rights(self.color).queen_side {
                        return None;
                    }

                    if self.game.board[Position::new(1, self.pos.y())].is_some() {
                        return None;
                    }

                    if self.game.board[Position::new(2, self.pos.y())].is_some() {
                        return None;
                    }

                    if self.game.board[Position::new(3, self.pos.y())].is_some() {
                        return None;
                    }

                    return Some(Position::new(2, self.pos.y()));
                }
                KingsMoves::None => return None,
            }
        }

        None
    }
}

/// Unifies the moves of a piece, independent of what piece exactly it is.
///
/// This enum implements the [`Mover`] trait, so use that to create it.
///
/// ```
/// # use chess_game::{Game, Position, game::{ PieceMove, Mover }};
///
/// let game = Game::new();
/// let moves: Vec<_> = PieceMove::new(
///     "g1".parse().unwrap(),
///     &game
/// ).collect();
/// assert!(moves.contains(&"f3".parse().unwrap()));
/// assert!(moves.contains(&"h3".parse().unwrap()));
/// ```
///
/// or, alternatively, see [`Game::possible_moves`]:
/// ```
/// # use chess_game::{Game, Position, game::{ PieceMove, Mover }};
///
/// let game = Game::new();
/// let moves: Vec<_> = game.possible_moves("g1".parse().expect("g1 is a valid position"))
///                         .expect("there is a piece at g1")
///                         .collect();
/// assert!(moves.contains(&"f3".parse().unwrap()));
/// assert!(moves.contains(&"h3".parse().unwrap()));
/// ```
#[derive(Debug, PartialEq, Eq)]
pub enum PieceMove<'a> {
    /// the piece is a pawn
    Pawn(PawnMove<'a>),
    /// the piece is a rook
    Rook(RookMove<'a>),
    /// the piece is a knight
    Knight(KnightMove<'a>),
    /// the piece is a bishop
    Bishop(BishopMove<'a>),
    /// the piece is a queen
    Queen(QueenMove<'a>),
    /// the piece is a king
    King(KingMove<'a>),
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
            crate::PieceKind::King => Self::King(KingMove::new_with_color(pos, game, color)),
        }
    }
}

impl<'a> Iterator for PieceMove<'a> {
    type Item = Position;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            PieceMove::Pawn(inner) => inner.next(),
            PieceMove::Rook(inner) => inner.next(),
            PieceMove::Knight(inner) => inner.next(),
            PieceMove::Bishop(inner) => inner.next(),
            PieceMove::Queen(inner) => inner.next(),
            PieceMove::King(inner) => inner.next(),
        }
    }
}

pub mod partial {
    use crate::{Game, Piece, Player, Position};

    use super::{
        directions::EightWayDirection, BishopMove, EightWayRotator, FourWayRotator, KingMove,
        KingsMoves, KnightMove, PawnDir, PawnMove, PieceMove, QueenMove, RookMove,
    };

    /// partial representation of a piece move, independent of game
    #[derive(Debug, Default)]
    pub struct PieceMovePartial {
        pos: Position,
        color: Player,
        rot: PartialRotation,
    }

    #[derive(Debug)]
    enum PartialRotation {
        /// the piece is a pawn
        Pawn {
            dir: Option<PawnDir>,
            is_blocked: bool,
            push_two: bool,
        },

        /// the piece is a rook
        Rook { rotator: FourWayRotator },
        /// the piece is a knight
        Knight { rotation: Option<EightWayDirection> },
        /// the piece is a bishop
        Bishop { rotator: FourWayRotator },
        /// the piece is a queen
        Queen { rotator: EightWayRotator },
        /// the piece is a king
        King { rotation: KingsMoves },
    }

    impl Default for PartialRotation {
        fn default() -> Self {
            Self::Knight { rotation: None }
        }
    }

    impl PieceMovePartial {
        pub(crate) fn from_full(p: PieceMove) -> Self {
            match p {
                PieceMove::Pawn(PawnMove {
                    pos,
                    game: _,
                    color,
                    push_two,
                    is_blocked,
                    dir,
                }) => Self {
                    pos,
                    color,
                    rot: PartialRotation::Pawn {
                        push_two,
                        dir,
                        is_blocked,
                    },
                },
                PieceMove::Rook(RookMove {
                    pos,
                    game: _,
                    rotator,
                    color,
                }) => Self {
                    pos,
                    color,
                    rot: PartialRotation::Rook { rotator },
                },
                PieceMove::Knight(KnightMove {
                    rotation,
                    pos,
                    game: _,
                    color,
                }) => Self {
                    pos,
                    color,
                    rot: PartialRotation::Knight { rotation },
                },
                PieceMove::Bishop(BishopMove {
                    pos,
                    game: _,
                    rotator,
                    color,
                }) => Self {
                    pos,
                    color,
                    rot: PartialRotation::Bishop { rotator },
                },
                PieceMove::Queen(QueenMove {
                    pos,
                    game: _,
                    rotator,
                    color,
                }) => Self {
                    pos,
                    color,
                    rot: PartialRotation::Queen { rotator },
                },
                PieceMove::King(KingMove {
                    rotation,
                    pos,
                    game: _,
                    color,
                }) => Self {
                    pos,
                    color,
                    rot: PartialRotation::King { rotation },
                },
            }
        }

        /// builds a partial representation from just a piece and a position
        pub fn new(_piece: Piece, _pos: Position) -> Self {
            todo!()
        }

        /// Returns `true` if the piece move is [`Pawn`].
        ///
        /// [`Pawn`]: PieceMove::Pawn
        #[must_use]
        pub fn is_pawn(&self) -> bool {
            matches!(self.rot, PartialRotation::Pawn { .. })
        }

        /// reconstructs the iterator
        pub fn build(self, game: &Game) -> PieceMove {
            let pos = self.pos;
            let color = self.color;
            match self.rot {
                PartialRotation::Pawn {
                    is_blocked,
                    dir,
                    push_two,
                } => PieceMove::Pawn(super::PawnMove {
                    pos,
                    game,
                    color,
                    is_blocked,
                    dir,
                    push_two,
                }),
                PartialRotation::Rook { rotator } => PieceMove::Rook(super::RookMove {
                    pos,
                    game,
                    rotator,
                    color,
                }),
                PartialRotation::Knight { rotation } => PieceMove::Knight(super::KnightMove {
                    pos,
                    game,
                    color,
                    rotation,
                }),
                PartialRotation::Bishop { rotator } => PieceMove::Bishop(super::BishopMove {
                    pos,
                    game,
                    color,
                    rotator,
                }),
                PartialRotation::Queen { rotator } => PieceMove::Queen(super::QueenMove {
                    pos,
                    game,
                    color,
                    rotator,
                }),
                PartialRotation::King { rotation } => PieceMove::King(super::KingMove {
                    pos,
                    game,
                    color,
                    rotation,
                }),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn reversibility() {
            let game: Game = "2r2rk1/pp3ppp/3pNq2/3P1b2/3p4/P3B3/1P1Q1PPP/R3K2R w KQ - 0 17"
                .parse()
                .unwrap();

            for (pos, piece) in game.pieces() {
                let mvs = PieceMove::new_with_piece(pos, &game, piece);
                assert_eq!(
                    mvs.partial().build(&game),
                    PieceMove::new_with_piece(pos, &game, piece)
                );
            }
        }
    }
}

impl<'a> PieceMove<'a> {
    /// creates a new [`PieceMove`] from the piece, taking its kind, color, and position
    pub fn new_with_piece(pos: Position, game: &'a Game, piece: Piece) -> Self {
        match piece.kind {
            crate::PieceKind::Pawn => Self::Pawn(PawnMove::new_with_color(pos, game, piece.color)),
            crate::PieceKind::Rook => Self::Rook(RookMove::new_with_color(pos, game, piece.color)),
            crate::PieceKind::Knight => {
                Self::Knight(KnightMove::new_with_color(pos, game, piece.color))
            }
            crate::PieceKind::Bishop => {
                Self::Bishop(BishopMove::new_with_color(pos, game, piece.color))
            }
            crate::PieceKind::Queen => {
                Self::Queen(QueenMove::new_with_color(pos, game, piece.color))
            }
            crate::PieceKind::King => Self::King(KingMove::new_with_color(pos, game, piece.color)),
        }
    }

    /// gets the player that is controlling the piece
    pub fn player(&self) -> Player {
        match self {
            PieceMove::Pawn(inner) => inner.color,
            PieceMove::Rook(inner) => inner.color,
            PieceMove::Knight(inner) => inner.color,
            PieceMove::Bishop(inner) => inner.color,
            PieceMove::Queen(inner) => inner.color,
            PieceMove::King(inner) => inner.color,
        }
    }

    /// constructs a new [PieceMove] with the same intermediate state but on a different game
    ///
    /// See also [`PieceMovePartial::build`].
    ///
    /// ```
    /// # use chess_game::{*, game::*};
    /// let mut game = Game::new();
    /// let mut piece_move = PieceMove::new("e2".parse().unwrap(), &game);
    ///
    /// piece_move.next();
    ///
    /// let partial = piece_move.partial();
    /// let move_info = game.try_make_move(Ply::parse_pure("e2e4").unwrap()).unwrap();
    /// game.unmake_move(move_info);
    ///
    /// let mut piece_move = partial.build(&game);
    ///
    /// # let mut before = PieceMove::new("e2".parse().unwrap(), &game);
    /// # before.next();
    /// // it remains unchanged
    /// assert_eq!(piece_move, before);
    ///
    /// // continue where we left off
    /// // even though the pawn moved in the meantime
    /// // the piece_move will be the exact same as if nothin happened
    /// piece_move.next();
    /// ```
    pub fn partial(self) -> PieceMovePartial {
        PieceMovePartial::from_full(self)
    }

    /// gets the starting position that the piece is coming from
    pub fn from(&self) -> Position {
        match self {
            PieceMove::Pawn(inner) => inner.pos,
            PieceMove::Rook(inner) => inner.pos,
            PieceMove::Knight(inner) => inner.pos,
            PieceMove::Bishop(inner) => inner.pos,
            PieceMove::Queen(inner) => inner.pos,
            PieceMove::King(inner) => inner.pos,
        }
    }

    /// Returns `true` if the piece move is [`Pawn`].
    ///
    /// [`Pawn`]: PieceMove::Pawn
    #[must_use]
    pub fn is_pawn(&self) -> bool {
        matches!(self, Self::Pawn(..))
    }

    /// Returns `true` if the piece move is [`Rook`].
    ///
    /// [`Rook`]: PieceMove::Rook
    #[must_use]
    pub fn is_rook(&self) -> bool {
        matches!(self, Self::Rook(..))
    }

    /// Returns `true` if the piece move is [`Knight`].
    ///
    /// [`Knight`]: PieceMove::Knight
    #[must_use]
    pub fn is_knight(&self) -> bool {
        matches!(self, Self::Knight(..))
    }

    /// Returns `true` if the piece move is [`Bishop`].
    ///
    /// [`Bishop`]: PieceMove::Bishop
    #[must_use]
    pub fn is_bishop(&self) -> bool {
        matches!(self, Self::Bishop(..))
    }

    /// Returns `true` if the piece move is [`Queen`].
    ///
    /// [`Queen`]: PieceMove::Queen
    #[must_use]
    pub fn is_queen(&self) -> bool {
        matches!(self, Self::Queen(..))
    }

    /// Returns `true` if the piece move is [`King`].
    ///
    /// [`King`]: PieceMove::King
    #[must_use]
    pub fn is_king(&self) -> bool {
        matches!(self, Self::King(..))
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use pretty_assertions::assert_eq;
    const PASSIVE_AGGRESSIVE_NOTE: &str = "i got this from chess.com, it better be valid";
    use super::*;

    /// generates a test case. The format is `testcase!([Mover struct], <name of test case>, <FEN
    /// string of the position>, <initial position> => <comma separated list of possible target
    /// squares>);`
    ///
    ///
    /// ## Example
    ///
    /// ```
    /// # use chess_game::game::KnightMove;
    /// testcase!(KnightMove, knight_move, "rn2kb1r/p1q1pp1p/2p2np1/1p6/3PB3/2N2N2/PPP2PPP/R1BQ1RK1 w kq - 2 10", "c3"
    ///         => "b5", "a4", "b1", "e2", "d5" );
    /// ```
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
        ($name:ident, $fen:literal, $start:literal => $($expected:literal),*) => {
            testcase!(PieceMove, $name, $fen, $start => $($expected,)*);
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

    testcase!(KingMove, long_castle, "rnbqkbnr/pp4pp/2pppp2/8/8/2NPB3/PPPQPPPP/R3KBNR w KQkq - 0 5", "e1"
        => "d1", "c1"
    );

    #[test]
    fn is_x_are_correct() {
        let game = Game::new();
        assert!(PieceMove::new("e2".parse().unwrap(), &game).is_pawn());
        assert!(PieceMove::new("a1".parse().unwrap(), &game).is_rook());
        assert!(PieceMove::new("b1".parse().unwrap(), &game).is_knight());
        assert!(PieceMove::new("c1".parse().unwrap(), &game).is_bishop());
        assert!(PieceMove::new("d1".parse().unwrap(), &game).is_queen());
        assert!(PieceMove::new("e1".parse().unwrap(), &game).is_king());
    }
}
