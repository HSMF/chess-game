#![doc = include_str!("../README.md")]
#![warn(missing_docs, missing_debug_implementations)]
use std::{
    cmp::Ordering,
    collections::VecDeque,
    fmt::{Debug, Display},
};
mod all_legal_moves;
mod eval;
#[cfg(test)]
/// Old implementations for searching moves, to check for validity of optimizations
mod old;
mod part_vec;

use all_legal_moves::AllLegalMovesEvaluating;
use chess_game::{
    game::MoveOutcome,
    Game, Player, Ply,
};
pub use eval::evaluate;

#[allow(unused)]
use itertools::Itertools;
use part_vec::PartVec;
use tinyvec::Array;

/// Evaluation of the position
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Eval {
    /// Someone has an advantage of {0} centipawns. The value will be negative for Black and
    /// positive for White
    Advantage(i64),
    /// the [Player] has forced mate in {1} plys
    MateIn(Player, u32),
}

impl Default for Eval {
    fn default() -> Self {
        Self::Advantage(0)
    }
}

impl Eval {
    fn bump_move_num(&self) -> Eval {
        match *self {
            Eval::Advantage(p) => Self::Advantage(p),
            Eval::MateIn(p, n) => Eval::MateIn(p, n + 1),
        }
    }

    /// returns true if self is strictly better for player than other
    fn better_for(&self, other: &Eval, player: Player) -> bool {
        match player {
            Player::Black => self < other,
            Player::White => other < self,
        }
    }

    /// returns true if self is better or equal for player than other
    fn better_for_eq(&self, other: &Eval, player: Player) -> bool {
        match player {
            Player::Black => self <= other,
            Player::White => other <= self,
        }
    }

    /// returns whether the eval is [`Eval::MateIn`] for the given player
    pub fn is_mate(&self, player: Player) -> bool {
        matches!(self, Eval::MateIn(p, _) if p == &player)
    }
}

impl Display for Eval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Eval::Advantage(ad) => {
                let ad = *ad as f64 / 100.0;
                write!(f, "{ad}")
            }
            Eval::MateIn(Player::White, n) => write!(f, "M{}", (n + 1) / 2),
            Eval::MateIn(Player::Black, n) => write!(f, "m{}", (n + 1) / 2),
        }
    }
}

impl PartialOrd for Eval {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Eval {
    fn cmp(&self, other: &Self) -> Ordering {
        use Eval::*;
        match (self, other) {
            (Advantage(a), Advantage(b)) => a.cmp(b),
            (Advantage(_), MateIn(Player::White, _)) => Ordering::Less,
            (Advantage(_), MateIn(Player::Black, _)) => Ordering::Greater,

            (MateIn(Player::White, _), Advantage(_)) => Ordering::Greater,
            (MateIn(Player::Black, _), Advantage(_)) => Ordering::Less,
            (MateIn(Player::White, _), MateIn(Player::Black, _)) => Ordering::Greater,
            (MateIn(Player::Black, _), MateIn(Player::White, _)) => Ordering::Less,
            (MateIn(Player::White, n), MateIn(Player::White, m)) => m.cmp(n),
            (MateIn(Player::Black, n), MateIn(Player::Black, m)) => n.cmp(m),
        }
    }
}

/// a container for reporting moves
pub trait Cont {
    /// the item contained
    type Item;
    /// iterator
    type Iter<'a>: IntoIterator<Item = &'a Self::Item>
    where
        Self: 'a;
    /// it has an empty constructor
    fn empty() -> Self;
    /// it has a way to add stuff
    fn add(&mut self, e: Self::Item);

    /// way to iterate over the elements
    fn iterate(&self) -> Self::Iter<'_>;
}

impl<T> Cont for Vec<T> {
    type Item = T;
    type Iter<'a> = std::iter::Rev<std::slice::Iter<'a, T>> where Self: 'a;
    fn empty() -> Self {
        Vec::new()
    }
    fn add(&mut self, e: T) {
        self.push(e);
    }

    fn iterate(&self) -> Self::Iter<'_> {
        self.iter().rev()
    }
}

impl<T> Cont for VecDeque<T> {
    type Item = T;
    type Iter<'a> = std::collections::vec_deque::Iter<'a, T> where Self: 'a;
    fn empty() -> Self {
        VecDeque::new()
    }
    fn add(&mut self, e: T) {
        self.push_front(e);
    }

    fn iterate(&self) -> Self::Iter<'_> {
        self.iter()
    }
}

impl<T> Cont for Option<T> {
    type Item = T;
    type Iter<'a> = std::option::Iter<'a, T> where Self: 'a;

    fn empty() -> Self {
        None
    }
    fn add(&mut self, e: Self::Item) {
        *self = Some(e);
    }

    fn iterate(&self) -> Self::Iter<'_> {
        self.iter()
    }
}

impl Cont for () {
    type Item = Ply;
    type Iter<'a> = std::iter::Empty<&'a Ply> where Self: 'a;

    fn empty() -> Self {}

    fn add(&mut self, _: Self::Item) {}

    fn iterate(&self) -> Self::Iter<'_> {
        std::iter::empty()
    }
}

impl<T, const N: usize> Cont for tinyvec::ArrayVec<[T; N]>
where
    [T; N]: Array,
{
    type Item = <[T; N] as Array>::Item;

    type Iter<'a> = std::iter::Rev<std::slice::Iter<'a, Self::Item>>
    where
        Self: 'a;

    fn empty() -> Self {
        Self::new()
    }

    fn add(&mut self, e: Self::Item) {
        self.push(e)
    }

    fn iterate(&self) -> Self::Iter<'_> {
        self.iter().rev()
    }
}

#[derive(Copy, Clone)]
struct AlphaBeta {
    alpha: Eval,
    beta: Eval,
}

impl AlphaBeta {
    fn new() -> Self {
        Self {
            alpha: Eval::MateIn(Player::Black, 1),
            beta: Eval::MateIn(Player::White, 1),
        }
    }

    fn update(&mut self, player: Player, best: Eval) {
        match player {
            Player::Black => {
                self.beta = std::cmp::min(self.beta, best);
            }
            Player::White => {
                self.alpha = std::cmp::max(self.alpha, best);
            }
        }
    }

    fn better_for(&self, player: Player, other: Eval) -> bool {
        match player {
            Player::Black => other < self.alpha,
            Player::White => other > self.beta,
        }
    }
}

impl Debug for AlphaBeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(ἀ = {}, 󰂡 = {})", self.alpha, self.beta)
    }
}

// because tinyvec::Array is not implemented for Arrays of size 69
struct DumbArray<T>([T; 69]);

impl<T> tinyvec::Array for DumbArray<T>
where
    T: Default,
{
    type Item = T;

    const CAPACITY: usize = 69;

    fn as_slice(&self) -> &[Self::Item] {
        self.0.as_slice()
    }

    fn as_slice_mut(&mut self) -> &mut [Self::Item] {
        self.0.as_mut_slice()
    }

    fn default() -> Self {
        DumbArray([(); 69].map(|_| Default::default()))
    }
}

fn alpha_beta_move_sorting<C>(
    game: &mut Game,
    depth: usize,
    max_depth: usize,
    mut alphabet: AlphaBeta,
) -> (Eval, C)
where
    C: Cont<Item = Ply>,
{
    match game.check_outcome() {
        MoveOutcome::CanClaimDraw => return (Eval::Advantage(0), C::empty()),
        MoveOutcome::Checkmate(p) => return (Eval::MateIn(p.other(), 0), C::empty()),
        MoveOutcome::None => {}
    };

    if depth >= max_depth {
        return (evaluate(game), C::empty());
    }

    let to_move = game.player_to_move();
    let mut best = (Eval::MateIn(to_move.other(), 1), C::empty());

    // [min avg max] == [0 18.294671069018403 68] for a sample game

    // [0.073 7.529611111111111 49.99] [s]
    // [0.08  8.25906944444445  47.4 ] [s]

    let mut legal_moves =
        AllLegalMovesEvaluating::new(game, to_move).collect::<PartVec<DumbArray<_>>>();
    match to_move {
        Player::Black => {
            legal_moves.sort_parts_by(|(_, a), (_, b)| a.cmp(b));
        }
        Player::White => {
            legal_moves.sort_parts_by(|(_, a), (_, b)| b.cmp(a));
        }
    }

    if depth == max_depth - 1 {
        return legal_moves
            .into_iter()
            .next()
            .map(|(_, b)| (b, C::empty()))
            .unwrap_or_else(|| (Eval::MateIn(to_move.other(), 0), C::empty()));
    }

    for (ply, _) in legal_moves.into_iter() {
        let Ok(move_info) = game.try_make_move(ply) else {continue};
        let (eval, mut moves) = alpha_beta_move_sorting::<C>(game, depth + 1, max_depth, alphabet);
        game.unmake_move(move_info);
        if eval.better_for_eq(&best.0, to_move) {
            moves.add(ply);
            best = (eval.bump_move_num(), moves);
        }
        if alphabet.better_for(to_move, best.0) {
            return best;
        }
        alphabet.update(to_move, best.0);
    }

    best
}

#[derive(Default, Debug)]
/// opaque struct that computes the best move
pub struct BestMove;

impl BestMove {
    /// computes the best move with some method
    pub fn best<C>(&mut self, game: &Game, depth: usize) -> (Eval, C)
    where
        C: Cont<Item = Ply>,
    {
        let mut game = game.clone();
        alpha_beta_move_sorting(&mut game, 0, depth, AlphaBeta::new())
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::old::min_max;

    use super::*;
    use chess_game::game::pgn::GameRecord;

    const CUTOFF: usize = 3;
    #[test]
    fn crashed() {
        let pgn = r#"[Event "?"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]

1. Nf3 {I've always thought this knight move is cool.} 1... f6 2. g4 Nc6 3. e4
Ne5 4. Nc3 Rb8 5. h3 Ra8 6. b3 Rb8 7. Bg2 Ra8 8. Nd4 Nc6 9. Nde2 h5 10. d3 hxg4
{Knocking them off.} 11. hxg4 Rxh1+ {I see it. I see it.} 12. Bxh1 *"#;
        reproduce_crash(pgn, 3);
    }

    fn reproduce_crash(pgn: &str, max_depth: usize) {
        let gr = GameRecord::parse(pgn).unwrap();
        let game = gr.build().unwrap();
        eprintln!("{game}");

        let (eval, best_move) = min_max(&game, max_depth);
        let best_move: Vec<_> = best_move;
        eprintln!("{eval} {best_move:?}");
    }

    #[test]
    fn crashed2() {
        let pgn = r#"[Event "?"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]

1. e4 {Best by test $1} 1... c6 {Can you Caro Kann $2} 2. g3 d5 3. e5 Qc7 4. a4
Qxe5+ {Knocking them off.} 5. Be2 Be6 6. f3 Nf6 7. f4 Qe4 8. h4 Qxh1 9. b3 *"#;
        reproduce_crash(pgn, 3);
    }

    mod m2 {
        use chess_game::Position;

        use crate::all_legal_moves::AllLegalMoves;

        use super::*;

        #[test]
        fn from_book() {
            let game: Game = "k7/1p5p/pPP5/P7/6p1/5pPp/5PpP/6K1 w - - 0 1"
                .parse()
                .unwrap();

            let mut operating_table = game.clone();

            AllLegalMoves::new(&mut operating_table, Player::Black).for_each(|x| eprint!("{x}, "));
            eprintln!();
            let (eval, best) = min_max::<Vec<_>>(&operating_table, 4);
            let best = best.iterate().collect_vec();
            assert_eq!(
                *best[0],
                Ply::Move {
                    from: Position::new(2, 5),
                    to: Position::new(2, 6),
                    promoted_to: None
                }
            );
            assert_eq!(best[2].to(Player::White), Position::new(2, 7),);
            assert_eq!(operating_table, game);
            assert_eq!(eval, Eval::MateIn(Player::White, 3));
            assert_eq!(eval.to_string(), "M2");
        }
    }

    fn valid_optimization<C: Cont<Item = Ply> + Eq + std::fmt::Debug>(
        pgn: &str,
        depth: usize,
        mut f: impl FnMut(&Game, usize) -> (Eval, C),
        mut g: impl FnMut(&Game, usize) -> (Eval, C),
    ) {
        let game_record = GameRecord::parse(pgn).unwrap();
        let mut game = Game::new();

        for &ply in game_record.moves() {
            game.try_make_move(ply).unwrap();
            eprintln!("========= '{game}' ==========");
            let slow = f(&game, depth);
            let fast = g(&game, depth);
            if slow != fast {
                eprintln!("{game}");
                eprintln!("{} {}", slow.0, fast.0);
                for (i, j) in slow
                    .1
                    .iterate()
                    .into_iter()
                    .zip(fast.1.iterate().into_iter())
                {
                    eprintln!("{i} {j}");
                }
            }
            // has to have same eval, might have two equivalent ways to get there
            assert_eq!(slow.0, fast.0);
        }
    }

    #[test]
    fn alpha_beta_valid() {
        const DEPTH: usize = 4;
        let pgn = r#"[Event "Live Chess"]
1. c4 e5 2. Nc3 Nf6 3. e4 c6 4. Qe2 Bc5 5. a3 O-O 6. b4 Bd6 7. c5 Bc7 8. Nf3 d5
9. exd5 Nxd5 10. Bb2 Nxc3 11. Bxc3 f6 12. d4 exd4 13. Qc4+ Kh8 14. Qxd4 Be5 15.
Qxd8 Rxd8 16. Bxe5 fxe5 17. Nxe5 Re8 18. f4 Nd7 19. O-O-O Nxe5 20. fxe5 Rxe5 21.
g3 Be6 22. Rd6 Bd5 23. Rg1 Rae8 24. Bd3 Re1+ 25. Rxe1 Rxe1+ 26. Kd2 Ra1 27. Rd8+
Bg8 28. Rb8 Ra2+ 29. Kc3 Rxa3+ 30. Kd4 b6 31. cxb6 axb6 32. Rxb6 Ra2 33. Rb8
Rxh2 34. Bc4 h6 35. Bxg8 Rd2+ 36. Kc5 Rc2+ 37. Kb6 Rc3 38. Rc8 Rxg3 39. Kxc6
Rg6+ 40. Kd7 Rf6 41. Ke7 Rf4 42. b5 Rb4 43. Rb8 Rxb5 44. Rxb5 Kxg8 45. Rh5 Kh7
46. Ke6 Kg6 47. Rc5 h5 48. Kd6 Kh6 49. Ke7 g6 50. Kf7 h4 51. Kf6 h3 52. Rg5 h2
53. Rxg6+ Kh7 54. Rg7+ Kh6 55. Rg6+ Kh7 56. Rg7+ Kh6 57. Rg6+ 1/2-1/2"#;
        valid_optimization::<tinyvec::ArrayVec<[_; DEPTH + 1]>>(
            pgn,
            4,
            |game, depth| min_max(game, depth),
            |game, depth| BestMove.best(game, depth),
        );
    }

    mod m3 {
        use super::*;
        #[test]
        fn puzzle() {
            let position = "r1b2qrk/ppp2p1p/3p1PpQ/6B1/2B1PR2/2P4P/P1P3P1/R5K1 w - - 0 1";
            let mut game: Game = position.parse().unwrap();

            let mate_in = 5;

            for i in (0..=mate_in).rev() {
                let (eval, best) = BestMove::default().best::<Option<_>>(&game, 5);
                assert_eq!(eval, Eval::MateIn(Player::White, i));
                eprintln!("{eval} -> {}", best.unwrap());
                game.try_make_move(best.expect("there is still a move"))
                    .expect("the move is valid");
                if game.check_outcome().is_checkmate() {
                    return;
                }
            }
            unreachable!("{position} should have mate sooner");
        }
    }

    #[test]
    fn no_knight_on_edge() {
        let positions: [Game; 3] = [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            "rnbqkbnr/pppppppp/8/8/8/N7/PPPPPPPP/R1BQKBNR b KQkq - 1 1",
            "rnbqkbnr/pppp1ppp/8/4p3/8/N7/PPPPPPPP/R1BQKBNR w KQkq - 0 2",
        ]
        .map(|x| x.parse().unwrap());

        assert!(evaluate(&positions[0]) > evaluate(&positions[1]));
        assert!(evaluate(&positions[0]) > evaluate(&positions[2]));

        let game: Game = "r1bqkbnr/pppppppp/n7/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 3 2"
            .parse()
            .unwrap();
        let (eval, mv) = min_max::<Vec<_>>(&game, 1);
        eprintln!("{eval} {}", mv.iterate().format(" -> "));
        assert!(eval.better_for(&Eval::Advantage(0), Player::Black));
    }

    #[test]
    fn d4_is_okay() {
        let mut game = Game::new();
        game.try_make_move(Ply::parse_pure("d2d4").unwrap())
            .unwrap();

        let eval = evaluate(&game);
        eprintln!("{eval}");
        assert!(!eval.better_for(&Eval::Advantage(0), Player::Black));
    }

    #[test]
    fn is_better_in_the_end() {
        let mut game: Game = Game::new();

        let (eval, best_move) = min_max::<Vec<_>>(&game, CUTOFF);

        for ply in best_move.iterate() {
            game.try_make_move(*ply).unwrap();
            eprintln!("{ply}: {}", evaluate(&game));
        }
        assert_eq!(evaluate(&game), eval);
        assert!(!eval.better_for(&Eval::Advantage(0), Player::Black));
    }

    #[test]
    fn eval_ord_as_expected() {
        use Eval::*;
        use Player::{Black as B, White as W};
        assert!(MateIn(W, 1) > MateIn(W, 12), "mate sooner is better");
        assert!(
            MateIn(W, 1) > MateIn(B, 0),
            "mate for white is better than mate for black"
        );
        assert!(
            MateIn(W, 0) > MateIn(W, 1),
            "being in checkmate is better than having to move"
        );
        assert!(MateIn(W, 1) > MateIn(W, 11289037), "mate sooner is better");
        assert!(
            MateIn(W, 12343) > Advantage(12300),
            "forced mate is always better"
        );
        assert!(MateIn(W, 12343) > Advantage(123));

        assert!(MateIn(B, 1) < MateIn(B, 12), "mate sooner is better");
        assert!(
            MateIn(B, 1) < MateIn(W, 0),
            "mate for black is better than mate for white"
        );
        assert!(
            MateIn(B, 0) < MateIn(B, 1),
            "being in checkmate is better than having to move"
        );
        assert!(MateIn(B, 1) < MateIn(B, 11289037), "mate sooner is better");
        assert!(
            MateIn(B, 12343) < Advantage(12300),
            "forced mate is always better"
        );
        assert!(MateIn(B, 12343) < Advantage(123));

        assert!(Advantage(-1) < Advantage(1));
    }

    #[test]
    fn finds_illegal_move() {
        let mut game: Game = "rnbqkb2/2P1n2r/p7/1p1p2p1/1P4pP/7B/PBPpN3/R2Q1K1R w q - 0 19"
            .parse()
            .unwrap();
        let mut best = BestMove::default();
        let (eval, moves) = best.best::<tinyvec::ArrayVec<[_; 7]>>(&game, 6);
        let ply = moves.iterate().next().unwrap();
        eprintln!("{eval}: {}", moves.iterate().format(", "));
        game.try_make_move(*ply).unwrap();
    }
}
