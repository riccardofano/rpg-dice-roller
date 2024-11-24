use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand::{rngs::StdRng, SeedableRng};
use rpg_dice_roller::{Dice, DiceKind, Expression};

pub fn benchmark_parsing(c: &mut Criterion) {
    c.bench_function("parse cursed dice", |b| {
        b.iter(|| Expression::parse(black_box("999d444")))
    });
    c.bench_function("parse multiple expressions", |b| {
        b.iter(|| Expression::parse(black_box("10d6 * (3dF + 3) / 100d%k2d6 + 100")))
    });
    c.bench_function("parse expression group", |b| {
        b.iter(|| Expression::parse(black_box("{10d6, 3dF, 100d%k2d6 + 100}")))
    });
}

pub fn benchmark_rolling(c: &mut Criterion) {
    c.bench_function("roll cursed dice", |b| {
        b.iter(|| {
            let dice = Dice::new(999, DiceKind::Standard(444), Vec::new());
            let mut rng = StdRng::seed_from_u64(1);
            dice.roll_all(&mut rng);
        });
    });
}

criterion_group!(benches, benchmark_parsing, benchmark_rolling);
criterion_main!(benches);
