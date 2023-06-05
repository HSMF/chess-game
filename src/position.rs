use std::{fmt::Display, ops::Add, str::FromStr};

use sdl2::rect::Rect;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct Position {
    x: u8,
    y: u8,
}

impl Position {
    pub fn new(x: u8, y: u8) -> Position {
        assert!(x < 8);
        assert!(y < 8);
        Position { x, y }
    }

    /// returns the file of the position
    pub fn x(&self) -> u8 {
        self.x
    }

    /// returns the rank of the position
    pub fn y(&self) -> u8 {
        self.y
    }

    /// returns the move distance between two positions. The move distance is defined as the
    /// minimum number of moves it takes for a king to move from a to b, assuming otherwise empty
    /// board
    pub fn distance(self, other: Self) -> u8 {
        let dx = std::cmp::max(self.x, other.x) - std::cmp::min(self.x, other.x);
        let dy = std::cmp::max(self.y, other.y) - std::cmp::min(self.y, other.y);
        std::cmp::max(dx, dy)
    }

    pub fn try_new(x: u8, y: u8) -> Option<Position> {
        if x < 8 && y < 8 {
            Some(Position { x, y })
        } else {
            None
        }
    }

    pub(crate) fn from_physical(x: u32, y: u32) -> Self {
        use crate::graphics::WIDTH;
        let x = (x / WIDTH) as u8;
        let y = ((8 * WIDTH - y) / WIDTH) as u8;
        Self { x, y }
    }

    pub(crate) fn to_rect(self) -> Rect {
        use crate::graphics::WIDTH;

        Rect::new(
            self.x as i32 * WIDTH as i32,
            7 * WIDTH as i32 - self.y as i32 * WIDTH as i32,
            WIDTH,
            WIDTH,
        )
    }

    pub fn file(&self) -> u8 {
        self.x
    }

    pub fn rank(&self) -> u8 {
        self.y
    }

    #[allow(unused)]
    #[must_use]
    pub fn as_tuple(self) -> (u8, u8) {
        (self.x, self.y)
    }
}

impl Add<(i8, i8)> for Position {
    type Output = Option<Position>;

    fn add(self, (dx, dy): (i8, i8)) -> Self::Output {
        let x = self.x.checked_add_signed(dx)?;
        let y = self.y.checked_add_signed(dy)?;
        Position::try_new(x, y)
    }
}

impl From<(u8, u8)> for Position {
    fn from(value: (u8, u8)) -> Self {
        Self::new(value.0, value.1)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Position is invalid")]
pub struct InvalidPosition;

impl FromStr for Position {
    type Err = InvalidPosition;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        let x = chars.next().ok_or(InvalidPosition)?;
        let x = match x {
            'a'..='h' => x as u8 - b'a',
            _ => return Err(InvalidPosition),
        };
        let y = chars.next().ok_or(InvalidPosition)?;
        let y = match y {
            '1'..='8' => y as u8 - b'1',
            _ => return Err(InvalidPosition),
        };
        Ok(Position { x, y })
    }
}

#[derive(Debug, thiserror::Error)]
#[error("index is not contained by board")]
struct NotOnBoard;

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = if self.x < 8 { self.x + b'a' } else { b'?' } as char;
        let y = if self.y < 8 { self.y + b'1' } else { b'?' } as char;

        write!(f, "{x}{y}")
    }
}
