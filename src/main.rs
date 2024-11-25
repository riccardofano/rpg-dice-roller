use rpg_dice_roller::{ComparePoint, Dice, DiceKind, Modifier};

fn main() {
    let dice = Dice::new(
        3,
        DiceKind::Standard(6),
        &[Modifier::TargetFailure(
            ComparePoint::GreaterThan(3.0),
            ComparePoint::LessThanOrEqual(3.0),
        )],
    );
    let output = dice.roll_all(&mut rand::thread_rng());

    println!("rolls: {}, sum: {}", output, output.value());
}
