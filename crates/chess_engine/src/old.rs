#![allow(unused)]

use chess_game::{Ply, Game, game::{PieceMove, MoveOutcome}, Player};

use crate::{Cont, Eval, evaluate, all_legal_moves::AllLegalMoves, AlphaBeta};

/// "best" move, very dumb
pub fn best_silly(game: &Game) -> Option<Ply> {
    let mut best_eval = Eval::MateIn(Player::White, 1);
    let mut best = None;
    for mover in game
        .pieces()
        .filter(move |(_, piece)| piece.player() == Player::Black)
        .map(|(pos, piece)| PieceMove::new_with_piece(pos, game, piece))
    {
        let mut game = game.clone();
        let from = mover.from();
        for to in mover {
            let Some(ply) = Ply::from_positions(from, to, None, &game) else {continue};

            if let Ok(move_info) = game.try_make_move(ply) {
                let eval = evaluate(&game);
                if eval < best_eval {
                    best_eval = eval;
                    best = Some(ply);
                }
                game.unmake_move(move_info);
            }
        }
    }

    best
}

/// silly little min_max implementation
pub fn min_max<C>(game: &Game, depth: usize) -> (Eval, C)
where
    C: Cont<Item = Ply>,
{
    let mut game = game.clone();
    min_max_rec(&mut game, 0, depth)
}

/// silly little min_max implementation
fn min_max_rec<C>(game: &mut Game, depth: usize, max_depth: usize) -> (Eval, C)
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

    let mut legal_moves = AllLegalMoves::new(game, to_move);

    while let Some(ply) = legal_moves.next() {
        let Ok(move_info) = legal_moves.game.try_make_move(ply) else {continue};
        let (eval, mut moves) = min_max_rec::<C>(legal_moves.game, depth + 1, max_depth);
        legal_moves.game.unmake_move(move_info);

        if eval.better_for_eq(&best.0, to_move) {
            moves.add(ply);
            best = (eval.bump_move_num(), moves);
        }
    }
    assert_eq!(game.player_to_move(), to_move);

    best
}

/// silly little min_max implementation
fn pruned_min_max<C>(
    game: &mut Game,
    depth: usize,
    max_depth: usize,
    previous_best: Eval,
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

    let mut legal_moves = AllLegalMoves::new(game, to_move);

    while let Some(ply) = legal_moves.next() {
        let Ok(move_info) = legal_moves.game.try_make_move(ply) else {continue};
        let (eval, mut ms) = pruned_min_max::<C>(legal_moves.game, depth + 1, max_depth, best.0);
        legal_moves.game.unmake_move(move_info);
        if eval.better_for(&best.0, to_move) {
            ms.add(ply);
            best = (eval.bump_move_num(), ms);
        }
        if best.0.better_for_eq(&previous_best, to_move) {
            return best;
        }
    }
    assert_eq!(game.player_to_move(), to_move);

    best
}

/// a bit faster impl of [`min_max`]
pub fn min_max_speedier<C>(game: &Game, depth: usize) -> (Eval, C)
where
    C: Cont<Item = Ply>,
{
    let player = game.player_to_move();
    let mut game = game.clone();
    pruned_min_max(&mut game, 0, depth, Eval::MateIn(player, 1))
}

#[allow(unused)]
fn alpha_beta<C>(
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

    let mut legal_moves = AllLegalMoves::new(game, to_move);

    while let Some(ply) = legal_moves.next() {
        let Ok(move_info) = legal_moves.game.try_make_move(ply) else {continue};
        let (eval, mut moves) = alpha_beta::<C>(legal_moves.game, depth + 1, max_depth, alphabet);
        legal_moves.game.unmake_move(move_info);
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
