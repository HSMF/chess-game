use std::io::{BufRead, Write};

use chess_game::{Game, Ply};

macro_rules! retry {
    ($e:expr, $game:expr) => {{
        match $e {
            Ok(c) => c,
            Err(e) => {
                eprintln!("{e}");
                print!("{:?} make a move: ", $game.player_to_move());
                std::io::stdout().flush().unwrap();
                continue;
            }
        }
    }};
}

fn main() {
    eprintln!("{:?}", chess_engine::evaluate(&Game::new()));
    let game = "rnbqkbnr/p1pppppp/8/1p6/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
        .parse()
        .unwrap();

    eprintln!("{:?}", chess_engine::evaluate(&game));

    let game = "rnbqkbnr/p2ppppp/8/1Bp5/4P3/8/PPPP1PPP/RNBQK1NR w KQkq - 0 3"
        .parse()
        .unwrap();

    eprintln!("{:?}", chess_engine::evaluate(&game));

    let mut game = Game::new();
    let stdin = std::io::stdin();
    println!("{game}");
    print!("{:?} make a move: ", game.player_to_move());
    std::io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let ply = retry!(Ply::parse_san(&line, &game), game);
        retry!(game.try_make_move(ply), game);
        println!("{game}");

        let engine_move = chess_engine::best_silly(&game).unwrap();
        eprintln!("{engine_move}");
        game.try_make_move(engine_move).unwrap();
        println!("{game}");

        print!("{:?} make a move: ", game.player_to_move());
        std::io::stdout().flush().unwrap();
    }
}
