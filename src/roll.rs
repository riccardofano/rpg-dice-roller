use rand::random;

use crate::parse::{Dice, DieKind};

pub fn roll(input: &str) -> String {
    // TODO: support multiple dice
    let dice = match Dice::parse(input) {
        Ok(d) => d,
        Err(e) => return e,
    };

    let mut rolled = Vec::with_capacity(dice.quantity as usize);
    for _ in 0..dice.quantity {
        rolled.push(roll_dice(random(), dice.kind));
    }

    format!("{:?}", rolled)
}

pub fn roll_dice(random_value: f32, die_kind: DieKind) -> i32 {
    match die_kind {
        DieKind::Standard(sides) => (random_value * sides as f32).ceil() as i32,
        DieKind::Fudge1 => [-1, 0, 0, 0, 0, 1][(random_value * 6_f32).floor() as usize],
        DieKind::Fudge2 => (random_value * 3_f32).floor() as i32 - 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rolling() {}
}
