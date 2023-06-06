use chess_game::{graphics, Game, Ply};
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
            print!("{:?} make a move: ", game.player_to_move());
            std::io::stdout().flush()?;
            for line in stdin.lock().lines() {
                let line = line?;
                let mut parse_and_run = || {
                    let ply = Ply::parse_san(&line, &game)?;
                    game.try_make_move(ply)?;
                    Ok::<_, anyhow::Error>(())
                };
                match parse_and_run() {
                    Ok(..) => {}
                    Err(e) => {
                        eprintln!("{e}");
                        print!("{:?} make a move: ", game.player_to_move());
                        std::io::stdout().flush()?;
                        continue;
                    }
                };
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
