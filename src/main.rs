use chess_game::{graphics, Game, Player, Ply};
use std::io::{BufRead, Write};

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
