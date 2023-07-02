use std::collections::HashMap;

use chess_game::{
    game::{pgn::GameRecord, MoveOutcome},
    Game, Player, Position,
};
use itertools::Itertools;
use pretty_assertions::assert_eq;

fn find_kings(game: &Game) -> (Position, Position) {
    let ((p1, k1), (p2, k2)) = game
        .pieces()
        .filter(|(_, x)| x.is_king())
        .collect_tuple()
        .expect("inconsistent amount of kings");
    match (k1.player(), k2.player()) {
        (Player::White, Player::Black) => (p1, p2),
        (Player::Black, Player::White) => (p2, p1),
        _ => panic!("inconsistent amounts of kings associated with colors"),
    }
}

#[test]
fn tester() -> std::io::Result<()> {
    for entry in std::fs::read_dir("./tests/cases/")? {
        let entry = entry?;

        if !entry.file_type()?.is_file() {
            continue;
        }

        let content = std::fs::read_to_string(entry.path())?;
        eprintln!("testing: {}", entry.path().display());

        let record = GameRecord::parse(&content).unwrap();
        let metadata = {
            let mut metadata = HashMap::new();
            for (k, v) in record.metadata() {
                metadata.insert(*k, *v);
            }
            metadata
        };
        let mut game = Game::new();
        let mut before = None::<Game>;
        for ply in record.moves() {
            eprintln!("{}", game.move_number());
            if let Some(before) = before {
                assert!(before.check_outcome().is_none());
            }
            let game_before = game.clone();
            let hash_before = game.hash();
            let mi = game.try_make_move(*ply).unwrap();

            game.unmake_move(mi);
            assert_eq!(game.hash(), hash_before);
            assert_eq!(game, game_before);

            game.try_make_move(*ply).unwrap();

            let (wk, bk) = find_kings(&game);
            assert_eq!(game.king_pos(Player::White), wk);
            assert_eq!(game.king_pos(Player::Black), bk);

            before = Some(game.clone());
        }

        if let Some(result) = metadata.get("TestResult") {
            match *result {
                "draw" => {
                    assert_eq!(game.check_outcome(), MoveOutcome::CanClaimDraw)
                }
                "white-win" => {
                    assert_eq!(
                        game.check_outcome(),
                        MoveOutcome::Checkmate(chess_game::Player::Black)
                    )
                }
                "black-win" => {
                    assert_eq!(
                        game.check_outcome(),
                        MoveOutcome::Checkmate(chess_game::Player::White)
                    )
                }
                "unknown" | "?" => {}
                x => {
                    panic!("invalid value for TestResult: \"{x}\"")
                }
            }
        }
    }

    Ok(())
}
