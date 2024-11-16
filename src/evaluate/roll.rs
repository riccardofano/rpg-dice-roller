use rand::Rng;
use std::fmt::Display;

use crate::parse::{ComparePoint, Dice, DiceKind, ExplodingKind, KeepKind, Modifier};

const MAX_ITERATIONS: usize = 1001;
const END_MODIFIER_CUTOFF: u8 = 9; // Everything after Modifier::Keep should be done after the rolls

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

const MODIFER_MOTATION: [&str; 16] = [
    "^", "v", "!", "!p", "!!", "!!p", "r", "ro", "u", "uo", "d", "d", "*", "_", "**", "__",
];

#[derive(Debug, Clone, Copy)]
pub struct Roll {
    pub(crate) value: i32,
    pub(crate) modifier_flags: u32,
}

struct RollsInfo {
    all: Vec<Roll>,
    current: Roll,
}

impl Dice {
    pub fn roll_all(&self, mut rng: impl Rng) -> Vec<Roll> {
        let mut rolls_info = RollsInfo {
            all: Vec::with_capacity(self.quantity as usize),
            current: Roll::new(0),
        };

        let first_post_roll_modifier = self
            .modifiers
            .iter()
            .position(|m| m.discriminant() >= END_MODIFIER_CUTOFF)
            .unwrap_or_else(|| self.modifiers.len() - 1);
        let (roll_modifiers, post_modifiers) = self.modifiers.split_at(first_post_roll_modifier);

        for _ in 0..self.quantity {
            rolls_info.current = Roll::new(self.roll(rng.gen()));

            for modifier in roll_modifiers {
                apply_modifier(self, *modifier, &mut rolls_info, &mut rng);
            }

            rolls_info.all.push(rolls_info.current);
        }

        for modifier in post_modifiers {
            apply_modifier(self, *modifier, &mut rolls_info, &mut rng);
        }

        rolls_info.all
    }

    pub fn roll(&self, random_value: f32) -> i32 {
        match self.kind {
            DiceKind::Standard(sides) => (random_value * sides as f32).ceil() as i32,
            DiceKind::Fudge1 => [-1, 0, 0, 0, 0, 1][(random_value * 6_f32).floor() as usize],
            DiceKind::Fudge2 => (random_value * 3_f32).floor() as i32 - 1,
        }
    }

    pub fn max_value(&self) -> i32 {
        match self.kind {
            DiceKind::Standard(sides) => sides as i32,
            DiceKind::Fudge1 => 1,
            DiceKind::Fudge2 => 1,
        }
    }

    pub fn min_value(&self) -> i32 {
        match self.kind {
            DiceKind::Standard(_) => 1,
            DiceKind::Fudge1 => -1,
            DiceKind::Fudge2 => -1,
        }
    }
}

fn apply_modifier(dice: &Dice, modifier: Modifier, rolls_info: &mut RollsInfo, rng: &mut impl Rng) {
    match modifier {
        Modifier::Min(min) => apply_min(min, &mut rolls_info.current),
        Modifier::Max(max) => apply_max(max, &mut rolls_info.current),
        Modifier::Exploding(exploding_kind, compare_point) => {
            apply_exploding(dice, rolls_info, rng, exploding_kind, compare_point)
        }
        Modifier::ReRoll(once, compare_point) => {
            apply_reroll(dice, rolls_info, rng, once, compare_point)
        }
        Modifier::Unique(once, compare_point) => {
            apply_unique(dice, rolls_info, rng, once, compare_point)
        }
        Modifier::TargetSuccess(compare_point) => apply_target_success(rolls_info, compare_point),
        Modifier::TargetFailure(compare_point) => apply_target_failure(rolls_info, compare_point),
        Modifier::CriticalSuccess(compare_point) => {
            apply_critical_success(dice, rolls_info, compare_point)
        }
        Modifier::CriticalFailure(compare_point) => {
            apply_critical_failure(dice, rolls_info, compare_point)
        }
        Modifier::Keep(keep_kind, amount) => apply_keep(rolls_info, keep_kind, amount),
        Modifier::Drop(keep_kind, amount) => apply_drop(rolls_info, keep_kind, amount),
        Modifier::Sort(sort_kind) => apply_sort(rolls_info, sort_kind),
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
    let should_explode: Box<dyn Fn(f64) -> bool> = match compare_point {
        Some(cmp) => cmp.compare_fn(),
        None => Box::new(|a| a == dice.max_value().into()),
    };

    match exploding_kind {
        ExplodingKind::Standard => {
            for _ in 0..MAX_ITERATIONS {
                if !should_explode(rolls_info.current.value.into()) {
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
            for _ in 0..MAX_ITERATIONS {
                if !should_explode(rolls_info.current.value.into()) {
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

            for _ in 0..MAX_ITERATIONS {
                if !should_explode(last_roll_value.into()) {
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

            for _ in 0..MAX_ITERATIONS {
                if !should_explode(last_roll_value.into()) {
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

fn apply_reroll(
    dice: &Dice,
    rolls_info: &mut RollsInfo,
    rng: &mut impl Rng,
    once: bool,
    compare_point: Option<ComparePoint>,
) {
    let should_reroll: Box<dyn Fn(f64) -> bool> = match compare_point {
        Some(cmp) => cmp.compare_fn(),
        None => Box::new(|a| a == dice.min_value().into()),
    };

    let (iterations, modifier_flag) = if once {
        (1, ModifierFlags::ReRollOnce)
    } else {
        (MAX_ITERATIONS, ModifierFlags::ReRoll)
    };
    for _ in 0..iterations {
        if !should_reroll(rolls_info.current.value.into()) {
            break;
        }

        rolls_info.current.set_modifier_flag(modifier_flag as u8);
        rolls_info.current.value = dice.roll(rng.gen());
    }
}

fn apply_unique(
    dice: &Dice,
    rolls_info: &mut RollsInfo,
    rng: &mut impl Rng,
    once: bool,
    compare_point: Option<ComparePoint>,
) {
    let should_reroll: Box<dyn Fn(f64) -> bool> = match compare_point {
        Some(cmp) => cmp.compare_fn(),
        None => Box::new(|_| true),
    };

    let (iterations, modifier_flag) = if once {
        (1, ModifierFlags::UniqueOnce)
    } else {
        (MAX_ITERATIONS, ModifierFlags::Unique)
    };

    for _ in 0..iterations {
        if !should_reroll(rolls_info.current.value.into())
            || !rolls_info
                .all
                .iter()
                .any(|r| r.value == rolls_info.current.value)
        {
            break;
        }

        rolls_info.current.set_modifier_flag(modifier_flag as u8);
        rolls_info.current.value = dice.roll(rng.gen());
    }
}

fn apply_target_success(rolls_info: &mut RollsInfo, compare_point: ComparePoint) {
    let cmp_fn = compare_point.compare_fn();

    if cmp_fn(rolls_info.current.value.into()) {
        rolls_info
            .current
            .set_modifier_flag(ModifierFlags::TargetSuccess as u8);
    }
}

fn apply_target_failure(rolls_info: &mut RollsInfo, compare_point: ComparePoint) {
    let cmp_fn = compare_point.compare_fn();

    if cmp_fn(rolls_info.current.value.into()) {
        rolls_info
            .current
            .set_modifier_flag(ModifierFlags::TargetFailure as u8);
    }
}

fn apply_critical_success(
    dice: &Dice,
    rolls_info: &mut RollsInfo,
    compare_point: Option<ComparePoint>,
) {
    let is_critical_success = match compare_point {
        Some(cmp) => cmp.compare_fn(),
        None => Box::new(|a| a == dice.max_value().into()),
    };

    if is_critical_success(rolls_info.current.value.into()) {
        rolls_info
            .current
            .set_modifier_flag(ModifierFlags::CriticalSuccess as u8);
    }
}

fn apply_critical_failure(
    dice: &Dice,
    rolls_info: &mut RollsInfo,
    compare_point: Option<ComparePoint>,
) {
    let is_critical_fail = match compare_point {
        Some(cmp) => cmp.compare_fn(),
        None => Box::new(|a| a == dice.min_value().into()),
    };

    if is_critical_fail(rolls_info.current.value.into()) {
        rolls_info
            .current
            .set_modifier_flag(ModifierFlags::CriticalFailure as u8);
    }
}

fn apply_keep(rolls_info: &mut RollsInfo, keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..rolls_info.all.len()).collect();
    match keep_kind {
        KeepKind::Highest => {
            indices.sort_by(|&ia, &ib| rolls_info.all[ib].value.cmp(&rolls_info.all[ia].value))
        }
        KeepKind::Lowest => {
            indices.sort_by(|&ia, &ib| rolls_info.all[ia].value.cmp(&rolls_info.all[ib].value))
        }
    }

    for &i in &indices[(amount as usize)..] {
        rolls_info.all[i].set_modifier_flag(ModifierFlags::Drop as u8);
    }
}

fn apply_drop(rolls_info: &mut RollsInfo, keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..rolls_info.all.len()).collect();
    match keep_kind {
        KeepKind::Highest => {
            indices.sort_by(|&ia, &ib| rolls_info.all[ib].value.cmp(&rolls_info.all[ia].value))
        }
        KeepKind::Lowest => {
            indices.sort_by(|&ia, &ib| rolls_info.all[ia].value.cmp(&rolls_info.all[ib].value))
        }
    }

    for &i in &indices[..(amount as usize)] {
        rolls_info.all[i].set_modifier_flag(ModifierFlags::Drop as u8);
    }
}

fn apply_sort(rolls_info: &mut RollsInfo, sort_kind: crate::parse::SortKind) {
    match sort_kind {
        crate::parse::SortKind::Ascending => rolls_info.all.sort(),
        crate::parse::SortKind::Descending => rolls_info.all.sort_by(|a, b| b.cmp(a)),
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

impl PartialEq for Roll {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl Eq for Roll {}
impl Ord for Roll {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}
impl PartialOrd for Roll {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Display for Roll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = format!("{}", self.value);
        for (i, notation) in MODIFER_MOTATION.iter().enumerate() {
            if self.was_modifier_applied(i as u8) {
                str.push_str(notation);

                if ModifierFlags::Drop as u8 == i as u8 {
                    // We don't want to apply critical success/failure on dropped rolls
                    break;
                }
            }
        }
        write!(f, "{}", str)
    }
}

fn to_notations(rolls: &[Roll]) -> String {
    format!(
        "[{}]",
        rolls
            .iter()
            .map(|r| r.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    )
}

impl ComparePoint {
    fn compare_fn(self) -> Box<dyn Fn(f64) -> bool> {
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
    use rand::{rngs::StdRng, SeedableRng};

    use crate::parse::SortKind;

    use super::*;

    fn values(rolls: &[Roll]) -> Vec<i32> {
        rolls.iter().map(|r| r.value).collect()
    }

    fn five_d6(modifiers: Vec<Modifier>) -> Dice {
        Dice {
            quantity: 5,
            kind: DiceKind::Standard(6),
            modifiers,
        }
    }

    // NOTE: First 20 rolls with rng seed set to 1
    // [5, 6, 5, 5, 2, 3, 2, 2, 5, 2, 4, 6, 5, 3, 6, 5, 1, 4, 1, 3]
    // Keep in mind that you usually set a roll before those

    fn test_rng() -> StdRng {
        StdRng::seed_from_u64(1)
    }

    fn empty_rolls(current: Roll) -> RollsInfo {
        RollsInfo {
            all: vec![],
            current,
        }
    }

    #[test]
    fn test_rolling() {
        let dice = five_d6(vec![Modifier::Min(3), Modifier::Keep(KeepKind::Highest, 2)]);
        let rolls = dice.roll_all(test_rng());

        assert_eq!(to_notations(&rolls), "[5, 6, 5d, 5d, 3^d]");
    }

    #[test]
    fn test_modifier_min() {
        let mut roll = Roll::new(2);
        apply_min(3, &mut roll);

        assert_eq!(roll.value, 3);
        assert!(roll.was_modifier_applied(ModifierFlags::Min as u8))
    }

    #[test]
    fn test_modifier_min_not_applied() {
        let mut roll = Roll::new(4);
        apply_min(3, &mut roll);

        assert_eq!(roll.value, 4);
        assert!(!roll.was_modifier_applied(ModifierFlags::Min as u8))
    }

    #[test]
    fn test_modifier_max() {
        let mut roll = Roll::new(4);
        apply_max(3, &mut roll);

        assert_eq!(roll.value, 3);
        assert!(roll.was_modifier_applied(ModifierFlags::Max as u8))
    }

    #[test]
    fn test_modifier_max_not_applied() {
        let mut roll = Roll::new(2);
        apply_max(3, &mut roll);

        assert_eq!(roll.value, 2);
        assert!(!roll.was_modifier_applied(ModifierFlags::Max as u8))
    }

    // NOTE: *This goes for all modifiers that add extra rolls*
    // Since the roll gets added to the rolls vec after all the modifiers have been applied
    // You have to check what's in the rolls + the current roll individually
    #[test]
    fn test_modifier_exploding() {
        let mut rolls_info = empty_rolls(Roll::new(6));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Standard,
            None,
        );

        assert_eq!(to_notations(&rolls_info.all), "[6!]");
        assert_eq!(rolls_info.current.value, 5);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingStandard as u8));
    }

    #[test]
    fn test_modifier_exploding_compare_point() {
        let mut rolls_info = empty_rolls(Roll::new(6));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Standard,
            Some(ComparePoint::GreaterThan(4.0)),
        );

        assert_eq!(to_notations(&rolls_info.all), "[6!, 5!, 6!, 5!, 5!]");
        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingStandard as u8));
    }

    #[test]
    fn test_modifier_exploding_compare_point_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(2));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Standard,
            Some(ComparePoint::GreaterThan(4.0)),
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingStandard as u8));
    }

    #[test]
    fn test_modifier_exploding_penetrating() {
        let mut rolls_info = empty_rolls(Roll::new(6));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Penetrating,
            None,
        );

        assert_eq!(to_notations(&rolls_info.all), "[6!p]");
        assert_eq!(rolls_info.current.value, 4);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingPenetrating as u8));
    }

    #[test]
    fn test_modifier_exploding_penetrating_compare_point() {
        let mut rolls_info = empty_rolls(Roll::new(6));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Penetrating,
            Some(ComparePoint::GreaterThan(4.0)),
        );

        assert_eq!(to_notations(&rolls_info.all), "[6!p]");
        assert_eq!(rolls_info.current.value, 4);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingPenetrating as u8));
    }

    #[test]
    fn test_modifier_exploding_penetrating_compare_point_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(2));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Penetrating,
            Some(ComparePoint::GreaterThan(4.0)),
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingPenetrating as u8));
    }

    #[test]
    fn test_modifier_exploding_compounding() {
        let mut rolls_info = empty_rolls(Roll::new(6));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Compounding,
            None,
        );

        assert_eq!(rolls_info.current.value, 11);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingCompounding as u8));
    }

    #[test]
    fn test_modifier_exploding_compounding_compare_point() {
        let mut rolls_info = empty_rolls(Roll::new(6));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Compounding,
            Some(ComparePoint::GreaterThan(4.0)),
        );

        assert_eq!(rolls_info.current.value, 29);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingCompounding as u8));
    }

    #[test]
    fn test_modifier_exploding_compounding_compare_point_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(2));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::Compounding,
            Some(ComparePoint::GreaterThan(4.0)),
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingCompounding as u8));
    }

    #[test]
    fn test_modifier_exploding_penetrating_compounding() {
        let mut rolls_info = empty_rolls(Roll::new(6));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::PenetratingCompounding,
            None,
        );

        assert_eq!(rolls_info.current.value, 10);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingPenetratingCompounding as u8));
    }

    #[test]
    fn test_modifier_exploding_penetrating_compounding_compare_point() {
        let mut rolls_info = empty_rolls(Roll::new(6));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::PenetratingCompounding,
            Some(ComparePoint::GreaterThan(3.0)),
        );

        assert_eq!(rolls_info.current.value, 24);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingPenetratingCompounding as u8));
    }

    #[test]
    fn test_modifier_exploding_penetrating_compounding_compare_point_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(5));

        apply_exploding(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            ExplodingKind::PenetratingCompounding,
            Some(ComparePoint::LessThan(4.0)),
        );

        assert_eq!(rolls_info.current.value, 5);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ExplodingPenetrating as u8));
    }

    #[test]
    fn test_modifier_reroll() {
        let mut rolls_info = empty_rolls(Roll::new(1));

        apply_reroll(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            false,
            None,
        );

        assert_eq!(rolls_info.current.value, 5);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ReRoll as u8));
    }

    #[test]
    fn test_modifier_reroll_compare_point() {
        let mut rolls_info = empty_rolls(Roll::new(4));

        apply_reroll(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            false,
            Some(ComparePoint::LessThanOrEqual(5.0)),
        );

        assert_eq!(rolls_info.current.value, 6);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ReRoll as u8));
    }

    #[test]
    fn test_modifier_reroll_compare_point_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(2));

        apply_reroll(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            false,
            Some(ComparePoint::Equal(4.0)),
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ReRoll as u8));
    }

    #[test]
    fn test_modifier_reroll_once() {
        let mut rolls_info = empty_rolls(Roll::new(1));

        apply_reroll(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            true,
            None,
        );

        assert_eq!(rolls_info.current.value, 5);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ReRollOnce as u8));
    }

    #[test]
    fn test_modifier_reroll_once_compare_point() {
        let mut rolls_info = empty_rolls(Roll::new(4));

        apply_reroll(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            true,
            Some(ComparePoint::LessThanOrEqual(5.0)),
        );

        assert_eq!(rolls_info.current.value, 5);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ReRollOnce as u8));
    }

    #[test]
    fn test_modifier_reroll_once_compare_point_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(2));

        apply_reroll(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            true,
            Some(ComparePoint::Equal(4.0)),
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::ReRollOnce as u8));
    }

    #[test]
    fn test_modifier_unique() {
        let rolls = vec![Roll::new(5), Roll::new(6)];
        let mut rolls_info = RollsInfo {
            all: rolls,
            current: Roll::new(5),
        };

        apply_unique(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            false,
            None,
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::Unique as u8));
    }

    #[test]
    fn test_modifier_unique_compare_point() {
        let rolls = vec![Roll::new(5), Roll::new(4)];
        let mut rolls_info = RollsInfo {
            all: rolls,
            current: Roll::new(5),
        };

        apply_unique(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            false,
            Some(ComparePoint::Equal(5.0)),
        );

        assert_eq!(rolls_info.current.value, 6);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::Unique as u8));
    }

    #[test]
    fn test_modifier_unique_compare_point_not_applied() {
        let rolls = vec![Roll::new(3), Roll::new(4)];
        let mut rolls_info = RollsInfo {
            all: rolls,
            current: Roll::new(5),
        };

        apply_unique(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            false,
            Some(ComparePoint::Equal(6.0)),
        );

        assert_eq!(rolls_info.current.value, 5);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::Unique as u8));
    }

    #[test]
    fn test_modifier_unique_compare_point_not_applied_because_unique() {
        let rolls = vec![Roll::new(3), Roll::new(4)];
        let mut rolls_info = RollsInfo {
            all: rolls,
            current: Roll::new(5),
        };

        apply_unique(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            false,
            Some(ComparePoint::Equal(5.0)),
        );

        assert_eq!(rolls_info.current.value, 5);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::Unique as u8));
    }

    #[test]
    fn test_modifier_unique_once() {
        let rolls = vec![Roll::new(5), Roll::new(6)];
        let mut rolls_info = RollsInfo {
            all: rolls,
            current: Roll::new(5),
        };

        apply_unique(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            true,
            None,
        );

        assert_eq!(rolls_info.current.value, 5);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::UniqueOnce as u8));
    }

    #[test]
    fn test_modifier_unique_once_compare_point() {
        let rolls = vec![Roll::new(5), Roll::new(6)];
        let mut rolls_info = RollsInfo {
            all: rolls,
            current: Roll::new(5),
        };

        apply_unique(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            true,
            Some(ComparePoint::LessThanOrEqual(5.0)),
        );

        assert_eq!(rolls_info.current.value, 5);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::UniqueOnce as u8));
    }

    #[test]
    fn test_modifier_unique_once_compare_point_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(2));

        apply_unique(
            &five_d6(vec![]),
            &mut rolls_info,
            &mut test_rng(),
            true,
            Some(ComparePoint::Equal(4.0)),
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::UniqueOnce as u8));
    }

    #[test]
    fn test_modifier_target_success() {
        let mut rolls_info = empty_rolls(Roll::new(2));
        apply_target_success(&mut rolls_info, ComparePoint::Equal(2.0));

        assert_eq!(rolls_info.current.value, 2);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::TargetSuccess as u8))
    }

    #[test]
    fn test_modifier_target_success_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(2));
        apply_target_success(&mut rolls_info, ComparePoint::Equal(3.0));

        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::TargetSuccess as u8))
    }

    #[test]
    fn test_modifier_target_failure() {
        let mut rolls_info = empty_rolls(Roll::new(1));
        apply_target_failure(&mut rolls_info, ComparePoint::LessThanOrEqual(2.0));

        assert_eq!(rolls_info.current.value, 1);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::TargetFailure as u8))
    }

    #[test]
    fn test_modifier_target_failure_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(4));
        apply_target_failure(&mut rolls_info, ComparePoint::GreaterThan(5.0));

        assert_eq!(rolls_info.current.value, 4);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::TargetFailure as u8))
    }

    #[test]
    fn test_modifier_critical_success() {
        let mut rolls_info = empty_rolls(Roll::new(6));
        apply_critical_success(&five_d6(vec![]), &mut rolls_info, None);

        assert_eq!(rolls_info.current.value, 6);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::CriticalSuccess as u8))
    }

    #[test]
    fn test_modifier_critical_success_compare_point() {
        let mut rolls_info = empty_rolls(Roll::new(2));
        apply_critical_success(
            &five_d6(vec![]),
            &mut rolls_info,
            Some(ComparePoint::Equal(2.0)),
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::CriticalSuccess as u8))
    }

    #[test]
    fn test_modifier_critical_success_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(2));
        apply_critical_success(
            &five_d6(vec![]),
            &mut rolls_info,
            Some(ComparePoint::Equal(3.0)),
        );

        assert_eq!(rolls_info.current.value, 2);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::CriticalSuccess as u8))
    }

    #[test]
    fn test_modifier_critical_failure() {
        let mut rolls_info = empty_rolls(Roll::new(1));
        apply_critical_failure(&five_d6(vec![]), &mut rolls_info, None);

        assert_eq!(rolls_info.current.value, 1);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::CriticalFailure as u8))
    }

    #[test]
    fn test_modifier_critical_failure_compare_point() {
        let mut rolls_info = empty_rolls(Roll::new(1));
        apply_critical_failure(
            &five_d6(vec![]),
            &mut rolls_info,
            Some(ComparePoint::LessThanOrEqual(2.0)),
        );

        assert_eq!(rolls_info.current.value, 1);
        assert!(rolls_info
            .current
            .was_modifier_applied(ModifierFlags::CriticalFailure as u8))
    }

    #[test]
    fn test_modifier_critical_failure_not_applied() {
        let mut rolls_info = empty_rolls(Roll::new(4));
        apply_critical_failure(
            &five_d6(vec![]),
            &mut rolls_info,
            Some(ComparePoint::GreaterThan(5.0)),
        );

        assert_eq!(rolls_info.current.value, 4);
        assert!(!rolls_info
            .current
            .was_modifier_applied(ModifierFlags::CriticalFailure as u8))
    }

    #[test]
    fn test_modifier_keep() {
        let mut rolls_info = RollsInfo {
            all: vec![Roll::new(1), Roll::new(2), Roll::new(3), Roll::new(4)],
            current: Roll::new(1),
        };

        apply_keep(&mut rolls_info, KeepKind::Highest, 2);

        assert_eq!(to_notations(&rolls_info.all), "[1d, 2d, 3, 4]");
    }

    #[test]
    fn test_modifier_keep_not_ordered() {
        let mut rolls_info = RollsInfo {
            all: vec![Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)],
            current: Roll::new(1),
        };

        apply_keep(&mut rolls_info, KeepKind::Highest, 2);

        assert_eq!(to_notations(&rolls_info.all), "[2d, 3, 4, 1d]");
    }

    #[test]
    fn test_modifier_keep_lowest_not_ordered() {
        let mut rolls_info = RollsInfo {
            all: vec![Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)],
            current: Roll::new(1),
        };

        apply_keep(&mut rolls_info, KeepKind::Lowest, 2);

        assert_eq!(to_notations(&rolls_info.all), "[2, 3d, 4d, 1]");
    }

    #[test]
    fn test_modifier_drop() {
        let mut rolls_info = RollsInfo {
            all: vec![Roll::new(1), Roll::new(2), Roll::new(3), Roll::new(4)],
            current: Roll::new(1),
        };

        apply_drop(&mut rolls_info, KeepKind::Highest, 3);

        assert_eq!(to_notations(&rolls_info.all), "[1, 2d, 3d, 4d]");
    }

    #[test]
    fn test_modifier_drop_not_ordered() {
        let mut rolls_info = RollsInfo {
            all: vec![Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)],
            current: Roll::new(1),
        };

        apply_drop(&mut rolls_info, KeepKind::Highest, 1);

        assert_eq!(to_notations(&rolls_info.all), "[2, 3, 4d, 1]");
    }

    #[test]
    fn test_modifier_drop_lowest_not_ordered() {
        let mut rolls_info = RollsInfo {
            all: vec![Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)],
            current: Roll::new(1),
        };

        apply_drop(&mut rolls_info, KeepKind::Lowest, 1);

        assert_eq!(to_notations(&rolls_info.all), "[2, 3, 4, 1d]");
    }

    #[test]
    fn test_modifier_sort_ascending() {
        let mut rolls_info = RollsInfo {
            all: vec![Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)],
            current: Roll::new(1),
        };

        apply_sort(&mut rolls_info, SortKind::Ascending);

        assert_eq!(values(&rolls_info.all), [1, 2, 3, 4]);
    }

    #[test]
    fn test_modifier_sort_descending() {
        let mut rolls_info = RollsInfo {
            all: vec![Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)],
            current: Roll::new(1),
        };

        apply_sort(&mut rolls_info, SortKind::Descending);

        assert_eq!(values(&rolls_info.all), [4, 3, 2, 1]);
    }
}
