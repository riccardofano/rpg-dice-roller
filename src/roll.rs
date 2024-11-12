use rand::Rng;

use crate::parse::{ComparePoint, Dice, DieKind, ExplodingKind, KeepKind, Modifier};

const MAX_ITERATIONS: usize = 1000;

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
enum ModifierFlags {
    Min,
    Max,
    ExplodingStandard,
    ExplodingPenetrating,
    ExplodingCompounding,
    ExplodingPenetratingCompounding,
    ReRoll,
    ReRollOnce,
    Unique,
    UniqueOnce,
    Drop,
    TargetSuccess,
    TargetFailure,
    CriticalSuccess,
    CriticalFailure,
}

#[derive(Debug, Clone, Copy)]
struct Roll {
    value: i32,
    modifier_flags: u32,
}

#[derive(Debug, Clone)]
struct DiceRolls {
    dice: Dice,
    rolls: Vec<Roll>,
}

struct RollsInfo {
    all: Vec<Roll>,
    current: Roll,
}

impl DiceRolls {
    pub fn roll_all(dice: Dice, mut rng: impl Rng) -> Self {
        let mut rolls_info = RollsInfo {
            all: Vec::with_capacity(dice.quantity as usize),
            current: Roll::new(0),
        };

        for _ in 0..dice.quantity {
            rolls_info.current = Roll::new(dice.roll(rng.gen()));

            for modifier in &dice.modifiers {
                // TODO: Keep/Drop/Sort should be applied last
                Self::apply_modifier(&dice, *modifier, &mut rolls_info, &mut rng);
            }

            rolls_info.all.push(rolls_info.current);
        }

        Self {
            rolls: rolls_info.all,
            dice,
        }
    }

    fn apply_modifier(
        dice: &Dice,
        modifier: Modifier,
        rolls_info: &mut RollsInfo,
        rng: &mut impl Rng,
    ) {
        match modifier {
            Modifier::Min(min) => Self::apply_min(min, &mut rolls_info.current),
            Modifier::Max(max) => Self::apply_max(max, &mut rolls_info.current),
            Modifier::Exploding(exploding_kind, compare_point) => {
                Self::apply_exploding(dice, rolls_info, rng, exploding_kind, compare_point)
            }
            _ => todo!(),
        }
    }

    fn apply_min(min: i32, roll: &mut Roll) {
        if min > roll.value {
            roll.value = min;
            roll.set_modifier_flag(ModifierFlags::Min as u8);
        }
    }

    fn apply_max(max: i32, roll: &mut Roll) {
        if max < roll.value {
            roll.value = max;
            roll.set_modifier_flag(ModifierFlags::Max as u8);
        }
    }

    fn apply_exploding(
        dice: &Dice,
        rolls_info: &mut RollsInfo,
        rng: &mut impl Rng,
        exploding_kind: ExplodingKind,
        compare_point: Option<ComparePoint>,
    ) {
        let should_explode: Box<dyn Fn(i32) -> bool> = match compare_point {
            Some(cmp) => cmp.compare_fn(),
            None => Box::new(|a| a == dice.max_value()),
        };

        match exploding_kind {
            ExplodingKind::Standard => {
                for _ in 0..MAX_ITERATIONS + 1 {
                    if !should_explode(rolls_info.current.value) {
                        break;
                    }

                    rolls_info
                        .current
                        .set_modifier_flag(ModifierFlags::ExplodingStandard as u8);
                    rolls_info.all.push(rolls_info.current);

                    let new_roll_value = dice.roll(rng.gen());
                    rolls_info.current = Roll::new(new_roll_value);
                }
            }
            ExplodingKind::Penetrating => {
                for _ in 0..MAX_ITERATIONS + 1 {
                    if !should_explode(rolls_info.current.value) {
                        break;
                    }

                    rolls_info
                        .current
                        .set_modifier_flag(ModifierFlags::ExplodingPenetrating as u8);
                    rolls_info.all.push(rolls_info.current);

                    let new_roll_value = dice.roll(rng.gen());
                    rolls_info.current = Roll::new(new_roll_value - 1);
                }
            }
            ExplodingKind::Compounding => {
                let mut last_roll_value = rolls_info.current.value;

                for _ in 0..MAX_ITERATIONS + 1 {
                    if !should_explode(last_roll_value) {
                        break;
                    }

                    rolls_info
                        .current
                        .set_modifier_flag(ModifierFlags::ExplodingCompounding as u8);
                    last_roll_value = dice.roll(rng.gen());
                    rolls_info.current.value += last_roll_value;
                }
            }
            ExplodingKind::PenetratingCompounding => {
                let mut last_roll_value = rolls_info.current.value;

                for _ in 0..MAX_ITERATIONS + 1 {
                    if !should_explode(last_roll_value) {
                        break;
                    }

                    rolls_info
                        .current
                        .set_modifier_flag(ModifierFlags::ExplodingPenetratingCompounding as u8);
                    last_roll_value = dice.roll(rng.gen()) - 1;
                    rolls_info.current.value += last_roll_value;
                }
            }
        }
    }
}

impl Dice {
    pub fn roll(&self, random_value: f32) -> i32 {
        match self.kind {
            DieKind::Standard(sides) => (random_value * sides as f32).ceil() as i32,
            DieKind::Fudge1 => [-1, 0, 0, 0, 0, 1][(random_value * 6_f32).floor() as usize],
            DieKind::Fudge2 => (random_value * 3_f32).floor() as i32 - 1,
        }
    }

    pub fn max_value(&self) -> i32 {
        match self.kind {
            DieKind::Standard(sides) => sides as i32,
            DieKind::Fudge1 => 1,
            DieKind::Fudge2 => 1,
        }
    }

    pub fn min_value(&self) -> i32 {
        match self.kind {
            DieKind::Standard(_) => 1,
            DieKind::Fudge1 => -1,
            DieKind::Fudge2 => -1,
        }
    }
}

impl Roll {
    fn new(value: i32) -> Self {
        Self {
            value,
            modifier_flags: 0,
        }
    }

    fn set_modifier_flag(&mut self, modifier_flag: u8) {
        self.modifier_flags |= 1 << modifier_flag;
    }

    fn was_modifier_applied(&self, modifier_flag: u8) -> bool {
        (self.modifier_flags & (1 << modifier_flag)) != 0
    }
}

impl ComparePoint {
    fn compare_fn(self) -> Box<dyn Fn(i32) -> bool> {
        match self {
            ComparePoint::Equal(n) => Box::new(move |a| a == n),
            ComparePoint::NotEqual(n) => Box::new(move |a| a != n),
            ComparePoint::LessThan(n) => Box::new(move |a| a < n),
            ComparePoint::GreaterThan(n) => Box::new(move |a| a > n),
            ComparePoint::LessThanOrEqual(n) => Box::new(move |a| a <= n),
            ComparePoint::GreaterThanOrEqual(n) => Box::new(move |a| a >= n),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parse::SortKind;

    #[test]
    fn test_rolling() {}
}
