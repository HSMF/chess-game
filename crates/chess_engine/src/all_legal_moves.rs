use std::marker::PhantomData;

use chess_game::{
    game::{PieceMove, PieceMovePartial, PiecesIterPartial},
    Game, PieceKind, Player, Ply, Position,
};

use crate::{evaluate, Eval};

pub trait Evaluator {
    type WithPly;
    fn obtain_evaluation(game: &Game) -> Self;
    fn combine_with_ply(self, ply: Ply) -> Self::WithPly;
}

impl Evaluator for () {
    type WithPly = Ply;

    fn obtain_evaluation(_: &Game) -> Self {}

    fn combine_with_ply(self, ply: Ply) -> Self::WithPly {
        ply
    }
}

impl Evaluator for Eval {
    type WithPly = (Ply, Eval);

    fn obtain_evaluation(game: &Game) -> Self {
        evaluate(game)
    }

    fn combine_with_ply(self, ply: Ply) -> Self::WithPly {
        (ply, self)
    }
}

pub type AllLegalMoves<'a> = AllLegalMovesIter<'a, ()>;
pub type AllLegalMovesEvaluating<'a> = AllLegalMovesIter<'a, Eval>;

#[derive(Debug)]
pub struct AllLegalMovesIter<'a, E: Evaluator> {
    pub game: &'a mut Game,
    color: Player,
    promote_to: Option<PieceKind>,
    pieces: PiecesIterPartial,
    mover: Option<PieceMovePartial>,
    mv: Option<(Position, Position)>,
    _evaluator: PhantomData<E>,
}

impl<E: Evaluator> AllLegalMovesIter<'_, E> {
    fn next_to_pos(&mut self) -> Option<(Position, Position)> {
        loop {
            if let Some(mover) = self.mover.take() {
                let mut mover = mover.build(self.game);
                let next = mover.next();

                if let Some(to) = next {
                    let from = mover.from();
                    self.mover = Some(mover.partial());
                    self.mv = Some((from, to));
                    return Some((from, to));
                }
            }

            // we don't currently have a mover, advance to next from position

            // cloning is cheap here
            let mut pieces = self.pieces.clone().build(self.game);
            let (pos, piece) = pieces.next()?;
            self.pieces = pieces.partial();
            if piece.player() != self.color {
                continue;
            }
            self.mover = Some(PieceMove::new_with_piece(pos, self.game, piece).partial());
        }
    }

    fn next_promotion(&mut self) -> Option<Ply> {
        match (self.mv, &self.mover) {
            (Some((from, to)), Some(mover))
                if mover.is_pawn() && to.y() == self.color.promotion_rank() =>
            {
                match self.promote_to {
                    None => None,
                    promoted_to @ Some(PieceKind::Queen) => {
                        self.promote_to = Some(PieceKind::Rook);
                        Some(Ply::Move {
                            from,
                            to,
                            promoted_to,
                        })
                    }
                    promoted_to @ Some(PieceKind::Rook) => {
                        self.promote_to = Some(PieceKind::Knight);
                        Some(Ply::Move {
                            from,
                            to,
                            promoted_to,
                        })
                    }
                    promoted_to @ Some(PieceKind::Knight) => {
                        self.promote_to = Some(PieceKind::Bishop);
                        Some(Ply::Move {
                            from,
                            to,
                            promoted_to,
                        })
                    }
                    promoted_to @ Some(PieceKind::Bishop) => {
                        self.promote_to = None;
                        Some(Ply::Move {
                            from,
                            to,
                            promoted_to,
                        })
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn verify_move(&mut self, ply: Ply) -> Option<E> {
        match self.game.try_make_move(ply) {
            Ok(mi) => {
                self.game.unmake_move(mi);
                Some(E::obtain_evaluation(self.game))
            }
            Err(_) => None,
        }
    }
}

impl<'a, E: Evaluator> Iterator for AllLegalMovesIter<'a, E> {
    type Item = E::WithPly;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ply) = self.next_promotion() {
                if let Some(eval) = self.verify_move(ply) {
                    return Some(eval.combine_with_ply(ply));
                }
            }

            let (from, to) = self.next_to_pos()?;
            if self.mover.as_ref().is_some_and(|x| x.is_pawn())
                && to.y() == self.color.promotion_rank()
            {
                self.promote_to = Some(PieceKind::Queen);
                continue;
            }

            let ply = Ply::from_positions(from, to, None, self.game)?;

            if let Some(eval) = self.verify_move(ply) {
                return Some(eval.combine_with_ply(ply));
            }
        }
    }
}

impl<'a, E: Evaluator> AllLegalMovesIter<'a, E> {
    pub fn new(game: &'a mut Game, color: Player) -> Self {
        Self {
            pieces: game.pieces().partial(),
            game,
            color,
            promote_to: None,
            mover: None,
            mv: None,
            _evaluator: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    fn pply(ply: &[Ply]) {
        eprint!("[");
        if let Some((first, rest)) = ply.split_first() {
            eprint!("{first}");
            for i in rest {
                eprint!(", {i}");
            }
        }
        eprintln!("]");
    }

    #[test]
    fn all_legal_moves_book() {
        let mut game: Game = "k7/1p5p/pPP5/P7/6p1/5pPp/5PpP/6K1 w - - 0 1"
            .parse()
            .unwrap();

        let moves = AllLegalMoves::new(&mut game, Player::White).collect::<HashSet<_>>();
        let expected = HashSet::from([
            Ply::parse_pure("c6c7").unwrap(),
            Ply::parse_pure("c6b7").unwrap(),
        ]);

        pply(moves.iter().copied().collect::<Vec<_>>().as_slice());
        pply(expected.iter().copied().collect::<Vec<_>>().as_slice());

        assert_eq!(moves, expected);
    }

    #[test]
    fn all_legal_moves_promotion() {
        let mut game: Game = "k7/1pP5/pP5p/P7/6p1/5pPp/5PpP/6K1 w - - 0 2"
            .parse()
            .unwrap();

        let moves = AllLegalMoves::new(&mut game, Player::White).collect::<HashSet<_>>();
        let expected = HashSet::from([
            Ply::parse_pure("c7c8q").unwrap(),
            Ply::parse_pure("c7b8r").unwrap(),
            Ply::parse_pure("c7b8b").unwrap(),
            Ply::parse_pure("c7b8n").unwrap(),
        ]);

        pply(moves.iter().copied().collect::<Vec<_>>().as_slice());
        pply(expected.iter().copied().collect::<Vec<_>>().as_slice());
    }

    #[test]
    fn all_legal_moves_is_consistent() {
        let mut game: Game = "rnbqkb2/2P1n2r/p7/1p1p2p1/1P4pP/7B/PBPpN3/R2Q1K1R w q - 0 19"
            .parse()
            .unwrap();
        game.try_make_move(Ply::parse_pure("a1b1").unwrap())
            .unwrap();
        game.try_make_move(Ply::parse_pure("g4g3").unwrap())
            .unwrap();
        game.try_make_move(Ply::parse_pure("b1c1").unwrap())
            .unwrap();
        game.try_make_move(Ply::parse_pure("g3g2").unwrap())
            .unwrap();

        let backup = game.clone();
        let mut all_legal_moves = AllLegalMoves::new(&mut game, Player::White);

        while let Some(_) = all_legal_moves.next() {
            eprintln!("{all_legal_moves:?}");
            assert_eq!(*all_legal_moves.game, backup);
        }
    }
}
