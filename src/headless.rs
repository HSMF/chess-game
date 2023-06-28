use std::time::Instant;

use chess_engine::{BestMove, Cont};
use itertools::Itertools;

pub fn main() -> anyhow::Result<()> {
    let mut game = chess_game::Game::new();
    let depth = std::env::args()
        .nth(2)
        .and_then(|x| x.parse().ok())
        .unwrap_or(5);

    const DEPTH: usize = 10;
    assert!(depth < DEPTH);
    println!("{game}");

    let total = Instant::now();
    let mut times = Vec::with_capacity(100);

    loop {
        let start = Instant::now();
        let mut best = BestMove::default();
        let (eval, best_move) = best.best::<tinyvec::ArrayVec<[_; DEPTH + 1]>>(&game, depth);
        let time = start.elapsed().as_millis() as f64 / 1000.0;
        eprintln!("took {time}");
        times.push(time);

        eprintln!(
            "my evaluation is : {eval}: {}.",
            best_move.iterate().format(", "),
        );
        game.try_make_move(*best_move.iterate().next().unwrap())
            .unwrap();
        println!("{game}");

        match game.check_outcome() {
            chess_game::game::MoveOutcome::None => {}
            chess_game::game::MoveOutcome::CanClaimDraw => {
                eprintln!("draw");
                break;
            }
            chess_game::game::MoveOutcome::Checkmate(p) => {
                eprintln!("{} won!", p.other());
                break;
            }
        }
    }

    eprintln!("total: {}", total.elapsed().as_millis() as f64 / 1000.0);
    let (min, max) = times.iter().minmax().into_option().unwrap();
    let avg = times.iter().sum::<f64>() / times.len() as f64;

    eprintln!("[{} {} {}] [s]", min, avg, max);

    Ok(())
}
