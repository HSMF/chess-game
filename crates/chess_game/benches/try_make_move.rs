use chess_game::{game::pgn::GameRecord, Game};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    const PGN: &str = r#"
1. e4 e5 2. Nc3 Nc6 3. Bc4 Bc5 4. d3 Nge7 5. Nf3 O-O 6. Nd5 Nxd5 7. Bxd5 Qf6 8.
a3 Nd4 9. Nxd4 Bxd4 10. Qd2 c6 11. c3 Bxc3 12. Qxc3 cxd5 13. exd5 d6 14. Be3 Bf5
15. d4 Rac8 16. Qd2 exd4 17. Qxd4 Qg6 18. O-O Bh3 19. g3 Bxf1 20. Rxf1 Qf5 21.
Qxa7 Qxd5 22. Bc1 Rc7 23. b4 Rfc8 24. Bb2 Qb3 25. Rb1 b6 26. Qxb6 Rc1+ 27. Rxc1
Rxc1+ 28. Bxc1 Qd1+ 29. Kg2 f6 30. Qd8+ Kf7 31. Be3 Kg6 32. Bf4 d5 33. b5 d4 34.
b6 d3 35. b7 Qe2 36. b8=Q 1-0
    "#;
    let record = GameRecord::parse(PGN).unwrap();

    let mut game: Game = Game::default();

    let mut group = c.benchmark_group("try_make_move");
    group.sample_size(1000);
    for &ply in record.moves() {
        let name = format!("try_make_move('{game}', '{ply}')");
        // imagine a very fidgety player
        group.bench_function(&name, |b| {
            b.iter(|| {
                let move_info = game.try_make_move(ply).unwrap();
                game.unmake_move(move_info);
            })
        });
        game.try_make_move(ply).unwrap();
    }
    group.finish()
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
