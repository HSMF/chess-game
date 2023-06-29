use std::io::{BufRead, BufReader, Write};
use std::process::Stdio;
use std::{process::Command, time::Instant};

use chess_engine::{BestMove, Cont, Eval};
use chess_game::{Game, Player, Ply};
use clap::Parser;
use itertools::Itertools;
use regex::Regex;

type Result<T> = anyhow::Result<T>;

const STOCKFISH_TIMEOUT_MS: usize = 2000;

#[derive(Parser)]
struct Opts {
    /// depth to run own engine at (default 5)
    #[clap(short, long)]
    depth: Option<usize>,
    /// whether stockfish should mirror timing, else it just takes its max timeout
    #[clap(short = 'M', long)]
    stockfish_mirror_timing: bool,
    /// the max timeout for stockfish in milliseconds, default is 2000ms
    #[clap(short = 'T', long)]
    stockfish_max_timeout: Option<usize>,
    /// some starting position, in FEN format
    #[clap(short, long)]
    starting_pos: Option<String>,
    /// whether to play against stockfish
    #[clap(short, long)]
    play_against_stockfish: bool,
}

fn get_score(s: &str, to_move: Player) -> Eval {
    let mut it = s.split(' ').take(2);
    let which = it.next().unwrap();
    let eval = it.next().unwrap();

    let eval: i64 = eval.parse().unwrap();
    match which {
        "mate" => {
            let player = if eval < 0 { to_move.other() } else { to_move };
            Eval::MateIn(player, eval.unsigned_abs() as u32)
        }
        "cp" => match to_move {
            // white is winning
            Player::Black if eval < 0 => Eval::Advantage(eval.abs()),
            // black is winning
            Player::Black => Eval::Advantage(-eval.abs()),
            // black is winning
            Player::White if eval < 0 => Eval::Advantage(-eval.abs()),
            // white is winning
            Player::White => Eval::Advantage(eval.abs()),
        },
        _ => unreachable!("invalid code"),
    }
}

struct StockfishFlags {
    mirror: bool,
    timeout: usize,
    own_time: usize,
}

fn best_stockfish(game: &Game, flags: StockfishFlags) -> Result<(Eval, Ply)> {
    let mut command = Command::new("stockfish")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = command.stdin.take().unwrap();

    let stdout = BufReader::new(command.stdout.take().unwrap());

    let timeout = if flags.mirror {
        std::cmp::min(flags.timeout, flags.own_time)
    } else {
        flags.timeout
    };

    writeln!(stdin, "isready")?;
    writeln!(stdin, "position fen {game}")?;
    writeln!(stdin, "go movetime {timeout}")?;

    let bestmove = Regex::new(r#"^bestmove (\w+)"#).unwrap();
    let score = Regex::new(r#"score (mate -?\d+|cp -?\d+)"#).unwrap();

    let mut last_best = None;
    for line in stdout.lines() {
        let line = line?;
        if let Some(cap) = score.captures(&line) {
            let eval = get_score(cap.get(1).unwrap().as_str(), game.player_to_move());
            last_best = Some(eval);
        }
        if let Some(cap) = bestmove.captures(&line) {
            let best = Ply::parse_pure(cap.get(1).unwrap().as_str()).unwrap();
            let eval = last_best.unwrap_or_default();
            command.kill()?;
            return Ok((eval, best));
        }
    }

    command.kill()?;
    panic!();
}

fn count_blunders(evals: &[Eval]) -> usize {
    evals
        .windows(2)
        .enumerate()
        .filter(|&(i, w)| {
            let to_move = if i % 2 == 0 {
                Player::White
            } else {
                Player::Black
            };
            match (w[0], w[1]) {
                (Eval::Advantage(before), Eval::Advantage(after)) => {
                    let diff = after - before;
                    match to_move {
                        Player::Black => diff > 100,
                        Player::White => diff < -100,
                    }
                }
                (Eval::Advantage(_), Eval::MateIn(p, n)) => {
                    if p == to_move {
                        false
                    } else {
                        n < 10
                    }
                }
                (Eval::MateIn(_, _), Eval::Advantage(_)) => true,
                (Eval::MateIn(_, _), Eval::MateIn(_, _)) => false,
            }
        })
        .count()
}

fn main() -> Result<()> {
    let opts = Opts::parse();
    let mut game = if let Some(pos) = opts.starting_pos {
        pos.parse().expect("must be a valid starting position")
    } else {
        Game::new()
    };
    let depth = opts.depth.unwrap_or(5);
    let sf_timeout = opts.stockfish_max_timeout.unwrap_or(STOCKFISH_TIMEOUT_MS);

    const DEPTH: usize = 10;
    assert!(depth < DEPTH);
    println!("{game}");

    let total = Instant::now();
    let mut times = Vec::with_capacity(100);

    let mut eval_records = Vec::with_capacity(100);

    let mut hit = 0usize;
    let mut delusions = vec![];
    let mut num_moves = 0usize;

    fn is_delusional(sf: Eval, own: Eval, depth: usize) -> bool {
        if sf == own {
            return false;
        }

        match (sf, own) {
            (Eval::Advantage(s), Eval::Advantage(o)) => (s - o).abs() >= 100,
            (Eval::Advantage(_), Eval::MateIn(_, _)) => {
                eprintln!("you must be delusional to think you can outsmart stockfish");
                true
            }
            // definitely delusional (doesn't see mate because of lacking depth yet)
            (Eval::MateIn(_, n), Eval::Advantage(_)) => n as usize <= depth,
            (Eval::MateIn(ps, n), Eval::MateIn(po, m)) => ps != po || m != n,
        }
    }

    loop {
        let start = Instant::now();
        let mut best = BestMove::default();
        let (eval, best_move) = best.best::<tinyvec::ArrayVec<[_; DEPTH + 1]>>(&game, depth);
        let own_time = start.elapsed().as_millis() as usize;
        let time = own_time as f64 / 1000.0;
        eprintln!("took {time}");
        times.push(time);
        let (eval_s, best_move_s) = best_stockfish(
            &game,
            StockfishFlags {
                own_time,
                mirror: opts.stockfish_mirror_timing,
                timeout: sf_timeout,
            },
        )?;
        eval_records.push(eval_s);
        if is_delusional(eval_s, eval, depth) {
            delusions.push(game.clone());
            eprintln!("engine is delusional");
        }

        eprintln!(
            "my          evaluation is : {eval}: {}.",
            best_move.iterate().format(", "),
        );
        eprintln!("stockfish's evaluation is : {eval_s}: {}.", best_move_s);

        if best_move_s == *best_move.iterate().next().unwrap() {
            hit += 1;
        }
        num_moves += 1;

        eprintln!(
            "{hit} / {num_moves} = {:.3}% accuracy",
            (hit as f64) / num_moves as f64 * 100.0
        );
        let delusional = delusions.len();
        println!(
            "was delusional {delusional} times: {:.3}% rate",
            delusional as f64 / num_moves as f64 * 100.0
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

        if opts.play_against_stockfish {
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
    }

    println!("total: {}s", total.elapsed().as_millis() as f64 / 1000.0);
    let (min, max) = times.iter().minmax().into_option().unwrap();
    let avg = times.iter().sum::<f64>() / times.len() as f64;

    println!("[{} {} {}] [s]", min, avg, max);
    eprintln!(
        "(timings are only for own moves, not for stockfish, which is ≤ {}ms)",
        sf_timeout
    );

    println!("depth: {depth}");
    println!(
        "{hit} / {num_moves} = {:.3}% accuracy",
        (hit as f64) / num_moves as f64 * 100.0
    );

    let delusional = delusions.len();
    println!(
        "was delusional {delusional} times: {:.3}% rate",
        delusional as f64 / num_moves as f64 * 100.0
    );
    println!("    {}", delusions.iter().format("\n    "));

    let delulu_score = 100 - (delusional as f64 / num_moves as f64 * 100.0) as u32;
    println!("delulu score for depth {depth}: {delulu_score}");

    eprintln!("{}", eval_records.iter().format(", "));
    eprintln!("{} blunders", count_blunders(&eval_records));

    Ok(())
}
