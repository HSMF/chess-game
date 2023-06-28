use std::hint::black_box;

use chess_game::{game::pgn::GameRecord, Game};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    {
        const PGN: &str = r#"
1. Nf3 f6 2. g4 Nc6 3. e4 Ne5 4. Nc3 Rb8 5. h3 Ra8
6. b3 Rb8 7. Bg2 Ra8 8. Nd4 Nc6 9. Nde2 h5 10. d3 hxg4
11. hxg4 Rxh1+ 12. Bxh1 *
    "#;
        let record = GameRecord::parse(PGN).unwrap();

        let mut game: Game = Game::default();

        let mut group = c.benchmark_group("check_outcome none");
        group.sample_size(1000);
        for &ply in record.moves() {
            let name = format!("check_outcome('{game}')");
            group.bench_function(&name, |b| {
                b.iter(|| {
                    let _ = black_box(game.check_outcome());
                })
            });
            game.try_make_move(ply).unwrap();
        }
        group.finish();
    }

    {
        let mut group = c.benchmark_group("check_outcome checkmate");
        group.sample_size(1000);
        let game: Game = "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
            .parse()
            .unwrap();
        let name = format!("check_outcome('{game}')");
        group.bench_function(&name, |b| {
            b.iter(|| {
                let _ = black_box(game.check_outcome());
            })
        });

        group.finish();
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
