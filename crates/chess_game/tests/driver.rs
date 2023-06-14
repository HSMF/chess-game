use std::collections::HashMap;

use chess_game::{
    game::{pgn::GameRecord, MoveOutcome},
    Game,
};

#[test]
fn tester() -> std::io::Result<()> {
    for entry in std::fs::read_dir("./tests/cases/")? {
        let entry = entry?;

        if !entry.file_type()?.is_file() {
            continue;
        }

        let content = std::fs::read_to_string(entry.path())?;

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
            if let Some(before) = before {
                assert!(before.check_outcome().is_none());
            }
            game.try_make_move(*ply).unwrap();
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
