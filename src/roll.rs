use crate::parse::{ComparePoint, Dice, DieKind, ExplodingKind, KeepKind, Modifier};
use rand::Rng;

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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parse::SortKind;

    #[test]
    fn test_rolling() {}
}
