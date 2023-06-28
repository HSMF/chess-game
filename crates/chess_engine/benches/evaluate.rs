use chess_engine::evaluate;
use chess_game::Game;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluate");
    group.sample_size(1000);
    let game: Game = Game::default();
    let name = format!("evaluate(&{game})");
    group.bench_function(&name, |b| b.iter(|| evaluate(&game)));
    group.finish()
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
