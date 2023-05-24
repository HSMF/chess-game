use crate::game::Game;
use std::{
    fmt::Display,
    io::{BufRead, Write},
};

use ply::Ply;

mod game;
mod graphics;
mod ply;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceKind {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Player {
    Black,
    White,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

fn main() -> anyhow::Result<()> {
    enum Mode {
        Cli,
        Gui,
    }
    let mode = std::env::args()
        .nth(1)
        .and_then(|x| match x.as_str() {
            "gui" => Some(Mode::Gui),
            "cli" => Some(Mode::Cli),
            _ => None,
        })
        .unwrap_or(Mode::Cli);

    match mode {
        Mode::Cli => {
            let mut game = Game::new();
            let stdin = std::io::stdin();
            println!("{game}");
            let mut player = Player::White;
            print!("{player:?} make a move: ");
            std::io::stdout().flush()?;
            for line in stdin.lock().lines() {
                let line = line?;
                let ply = match Ply::parse_san(&line, &game.board, player) {
                    Ok(ply) => ply,
                    Err(e) => {
                        eprintln!("{e}");
                        print!("{player:?} make a move: ");
                        std::io::stdout().flush()?;
                        continue;
                    }
                };
                player.flip();
                game.board.make_move(ply)?;
                println!("{game}");

                print!("{player:?} make a move: ");
                std::io::stdout().flush()?;
            }
        }
        Mode::Gui => {
            graphics::main()?;
        }
    }

    Ok(())
}
