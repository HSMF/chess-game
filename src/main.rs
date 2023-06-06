use chess_game::{graphics, Game, Ply};
use std::io::{BufRead, Write};

macro_rules! retry {
    ($e:expr, $game:expr) => {{
        match $e {
            Ok(c) => {c}
            Err(e) => {
                eprintln!("{e}");
                print!("{:?} make a move: ", $game.player_to_move());
                std::io::stdout().flush()?;
                continue;
            }
        }
    }};
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
            print!("{:?} make a move: ", game.player_to_move());
            std::io::stdout().flush()?;
            for line in stdin.lock().lines() {
                let line = line?;
                let ply = retry!(Ply::parse_san(&line, &game), game);
                retry!(game.try_make_move(ply), game);
                println!("{game}");

                print!("{:?} make a move: ", game.player_to_move());
                std::io::stdout().flush()?;
            }
        }
        Mode::Gui => {
            graphics::main()?;
        }
    }

    Ok(())
}
