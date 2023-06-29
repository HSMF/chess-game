//! Only for measuring performance of any functionality

use std::{
    fmt::Display,
    hint::black_box,
    time::{Duration, Instant},
};

use chess_game::Game;

struct Time(Duration);

impl Display for Time {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.as_nanos() <= 1000 {
            write!(f, "{}ns", self.0.as_nanos())
        } else if self.0.as_micros() <= 1000 {
            write!(f, "{}µs", self.0.as_nanos() as f64 / 1000.0)
        } else if self.0.as_millis() <= 1000 {
            write!(f, "{}ms", self.0.as_micros() as f64 / 1000.0)
        } else {
            write!(f, "{}s", self.0.as_millis() as f64 / 1000.0)
        }
    }
}

fn main() {
    let game: Game = "r1bqkbnr/pppp1Qp1/2n4p/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
        .parse()
        .unwrap();

    let now = Instant::now();
    const N: usize = 400_000;

    for _ in 0..N {
        let _ = black_box(game.check_outcome());
    }

    let elapsed = Time(now.elapsed());

    let per_iteration = Time(elapsed.0.checked_div(N as u32).unwrap());

    println!("took {elapsed}. That's on average {per_iteration}");
}
