use rand::{thread_rng, Rng};

use super::roll::{ModifierFlags, Roll, RollOutput, RollOutputKind};
use crate::parse::{ComparePoint, Dice, DiceKind, ExplodingKind, KeepKind, Modifier, SortKind};

const MAX_ITERATIONS: usize = 1001;

struct RollsInfo {
    all: Vec<Roll>,
    current: Roll,
}

impl Dice {
    /// Creates a new dice.
    /// The quantity will clamped between 1 and 999 if a number outside that range is passed in.
    /// The modifiers will be sorted in the order specified by the enum and only
    /// the last one of each variant will be applied.
    pub fn new(quantity: u32, kind: DiceKind, modifiers: &[Modifier]) -> Self {
        let quantity = quantity.clamp(1, 999);
        Self {
            quantity,
            kind,
            modifiers: Modifier::filter(modifiers),
        }
    }

    pub fn quantity(&self) -> u32 {
        self.quantity
    }
    pub fn modifiers(&self) -> &[Modifier] {
        &self.modifiers
    }
    pub fn sides(&self) -> u32 {
        match self.kind {
            DiceKind::Standard(sides) => sides,
            DiceKind::Fudge1 | DiceKind::Fudge2 => 6,
        }
    }
    pub fn kind(&self) -> DiceKind {
        self.kind
    }
    pub fn max_value(&self) -> i32 {
        match self.kind {
            DiceKind::Standard(sides) => sides as i32,
            DiceKind::Fudge1 => 1,
            DiceKind::Fudge2 => 1,
        }
    }
    fn max_value_f64(&self) -> f64 {
        self.max_value() as f64
    }
    pub fn min_value(&self) -> i32 {
        match self.kind {
            DiceKind::Standard(_) => 1,
            DiceKind::Fudge1 => -1,
            DiceKind::Fudge2 => -1,
        }
    }
    fn min_value_f64(&self) -> f64 {
        self.min_value() as f64
    }

    fn roll_amount(&self, amount: usize, rng: &mut impl Rng) -> RollOutput {
        let mut rolls_info = RollsInfo {
            all: Vec::with_capacity(amount),
            current: Roll::new(0),
        };

        let (roll_modifiers, post_modifiers) =
            Modifier::split_roll_and_output_modifiers(&self.modifiers);

        for _ in 0..amount {
            rolls_info.current = Roll::new(self.roll_value(rng));

            for modifier in roll_modifiers {
                apply_modifier(self, *modifier, &mut rolls_info, rng);
            }

            rolls_info.all.push(rolls_info.current);
        }

        let mut output_kind = RollOutputKind::Sum;
        for modifier in post_modifiers {
            apply_modifier(self, *modifier, &mut rolls_info, rng);

            match modifier {
                Modifier::TargetSuccess(_) => output_kind = RollOutputKind::TargetSuccess,
                Modifier::TargetFailure(_, _) => output_kind = RollOutputKind::TargetFailure,
                _ => {}
            }
        }

        RollOutput::new(rolls_info.all, output_kind)
    }

    /// Roll the full quantity of the dice.
    /// Uses rand::thread_rng(), if you want to choose the rng yourself use `roll_all_with()`
    pub fn roll_all(&self) -> RollOutput {
        self.roll_amount(self.quantity as usize, &mut thread_rng())
    }

    /// Roll the full quantity of the dice with the rng specified.
    /// If you don't care about which rng to use you can use `roll_all` instead.
    pub fn roll_all_with(&self, rng: &mut impl Rng) -> RollOutput {
        self.roll_amount(self.quantity as usize, rng)
    }

    /// Roll the dice only once.
    /// Uses rand::thread_rng(), if you want to choose the rng yourself use `roll_once_with()`
    pub fn roll_once(&self) -> RollOutput {
        self.roll_amount(1, &mut thread_rng())
    }

    /// Roll the dice only once with the rng specified.
    /// If you don't care about which rng to use you can use `roll_once` instead.
    pub fn roll_once_with(&self, rng: &mut impl Rng) -> RollOutput {
        self.roll_amount(1, rng)
    }

    fn roll_value(&self, rng: &mut impl Rng) -> i32 {
        let random_value: f32 = rng.gen();

        match self.kind {
            DiceKind::Standard(sides) => (random_value * sides as f32).ceil() as i32,
            DiceKind::Fudge1 => [-1, 0, 0, 0, 0, 1][(random_value * 6_f32).floor() as usize],
            DiceKind::Fudge2 => (random_value * 3_f32).floor() as i32 - 1,
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
        Modifier::TargetSuccess(compare_point) => {
            apply_target_success(&mut rolls_info.all, compare_point)
        }
        Modifier::TargetFailure(success_cmp, failure_cmp) => {
            apply_target_failure(&mut rolls_info.all, success_cmp, failure_cmp)
        }
        Modifier::CriticalSuccess(compare_point) => {
            apply_critical_success(dice, &mut rolls_info.all, compare_point)
        }
        Modifier::CriticalFailure(compare_point) => {
            apply_critical_failure(dice, &mut rolls_info.all, compare_point)
        }
        Modifier::Keep(keep_kind, amount) => apply_keep(&mut rolls_info.all, keep_kind, amount),
        Modifier::Drop(keep_kind, amount) => apply_drop(&mut rolls_info.all, keep_kind, amount),
        Modifier::Sort(sort_kind) => apply_sort(&mut rolls_info.all, sort_kind),
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
        Some(cmp) => {
            if !cmp.can_dice_pass(dice) {
                return;
            }
            cmp.compare_fn()
        }
        None => Box::new(|a| a == dice.max_value_f64()),
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

                let new_roll_value = dice.roll_value(rng);
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

                let new_roll_value = dice.roll_value(rng);
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
                last_roll_value = dice.roll_value(rng);
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
                last_roll_value = dice.roll_value(rng) - 1;
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
        Some(cmp) => {
            if !cmp.can_dice_pass(dice) {
                return;
            }
            cmp.compare_fn()
        }
        None => Box::new(|a| a == dice.min_value_f64()),
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
        rolls_info.current.value = dice.roll_value(rng);
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
        Some(cmp) => {
            if !cmp.can_dice_pass(dice) {
                return;
            }
            cmp.compare_fn()
        }
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
        rolls_info.current.value = dice.roll_value(rng);
    }
}

fn apply_target_success(rolls: &mut [Roll], compare_point: ComparePoint) {
    let cmp_fn = compare_point.compare_fn();

    for roll in rolls.iter_mut() {
        if cmp_fn(f64::from(roll.value)) {
            roll.set_modifier_flag(ModifierFlags::TargetSuccess as u8);
        }
    }
}

fn apply_target_failure(rolls: &mut [Roll], success_cmp: ComparePoint, failure_cmp: ComparePoint) {
    apply_target_success(rolls, success_cmp);

    let cmp_fn = failure_cmp.compare_fn();
    for roll in rolls.iter_mut() {
        if cmp_fn(f64::from(roll.value)) {
            roll.set_modifier_flag(ModifierFlags::TargetFailure as u8);
        }
    }
}

fn apply_critical_success(dice: &Dice, rolls: &mut [Roll], compare_point: Option<ComparePoint>) {
    let is_critical_success = match compare_point {
        Some(cmp) => cmp.compare_fn(),
        None => Box::new(|a| a == dice.max_value_f64()),
    };

    for roll in rolls.iter_mut() {
        if is_critical_success(f64::from(roll.value)) {
            roll.set_modifier_flag(ModifierFlags::CriticalSuccess as u8);
        }
    }
}

fn apply_critical_failure(dice: &Dice, rolls: &mut [Roll], compare_point: Option<ComparePoint>) {
    let is_critical_fail = match compare_point {
        Some(cmp) => cmp.compare_fn(),
        None => Box::new(|a| a == dice.min_value_f64()),
    };

    for roll in rolls.iter_mut() {
        if is_critical_fail(f64::from(roll.value)) {
            roll.set_modifier_flag(ModifierFlags::CriticalFailure as u8);
        }
    }
}

fn apply_keep(rolls: &mut [Roll], keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..rolls.len()).collect();
    match keep_kind {
        KeepKind::Highest => indices.sort_by(|&ia, &ib| rolls[ib].value.cmp(&rolls[ia].value)),
        KeepKind::Lowest => indices.sort_by(|&ia, &ib| rolls[ia].value.cmp(&rolls[ib].value)),
    }

    for &i in &indices[(amount as usize)..] {
        rolls[i].set_modifier_flag(ModifierFlags::Drop as u8);
    }
}

fn apply_drop(rolls: &mut [Roll], keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..rolls.len()).collect();
    match keep_kind {
        KeepKind::Highest => indices.sort_by(|&ia, &ib| rolls[ib].value.cmp(&rolls[ia].value)),
        KeepKind::Lowest => indices.sort_by(|&ia, &ib| rolls[ia].value.cmp(&rolls[ib].value)),
    }

    for &i in &indices[..(amount as usize)] {
        rolls[i].set_modifier_flag(ModifierFlags::Drop as u8);
    }
}

fn apply_sort(rolls: &mut [Roll], sort_kind: SortKind) {
    match sort_kind {
        SortKind::Ascending => rolls.sort(),
        SortKind::Descending => rolls.sort_by(|a, b| b.cmp(a)),
    }
}

pub fn to_notations(rolls: &[Roll]) -> String {
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
    pub fn compare_fn(self) -> Box<dyn Fn(f64) -> bool> {
        match self {
            ComparePoint::Equal(n) => Box::new(move |a| a == n),
            ComparePoint::NotEqual(n) => Box::new(move |a| a != n),
            ComparePoint::LessThan(n) => Box::new(move |a| a < n),
            ComparePoint::GreaterThan(n) => Box::new(move |a| a > n),
            ComparePoint::LessThanOrEqual(n) => Box::new(move |a| a <= n),
            ComparePoint::GreaterThanOrEqual(n) => Box::new(move |a| a >= n),
        }
    }

    pub fn can_dice_pass(self, dice: &Dice) -> bool {
        match self {
            ComparePoint::Equal(n) => dice.min_value_f64() <= n && n <= dice.max_value_f64(),
            ComparePoint::NotEqual(n) => !(dice.min_value_f64() <= n && n <= dice.max_value_f64()),
            ComparePoint::LessThan(n) => n > dice.min_value_f64(),
            ComparePoint::GreaterThan(n) => n < dice.max_value_f64(),
            ComparePoint::LessThanOrEqual(n) => n >= dice.min_value_f64(),
            ComparePoint::GreaterThanOrEqual(n) => n <= dice.max_value_f64(),
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
        let rolls = dice.roll_all_with(&mut test_rng());

        assert_eq!(to_notations(&rolls.rolls), "[5, 6, 5d, 5d, 3^d]");
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
            &mut &mut test_rng(),
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
        let mut rolls = [Roll::new(2)];
        apply_target_success(&mut rolls, ComparePoint::Equal(2.0));

        assert_eq!(rolls[0].value, 2);
        assert!(rolls[0].was_modifier_applied(ModifierFlags::TargetSuccess as u8))
    }

    #[test]
    fn test_modifier_target_success_not_applied() {
        let mut rolls = [Roll::new(2)];
        apply_target_success(&mut rolls, ComparePoint::Equal(3.0));

        assert_eq!(rolls[0].value, 2);
        assert!(!rolls[0].was_modifier_applied(ModifierFlags::TargetSuccess as u8))
    }

    #[test]
    fn test_modifier_target_failure() {
        let mut rolls = [Roll::new(1)];
        apply_target_failure(
            &mut rolls,
            ComparePoint::GreaterThan(5.0),
            ComparePoint::LessThanOrEqual(2.0),
        );

        assert_eq!(rolls[0].value, 1);
        assert!(rolls[0].was_modifier_applied(ModifierFlags::TargetFailure as u8))
    }

    #[test]
    fn test_modifier_target_failure_success_applied() {
        let mut rolls = [Roll::new(6)];
        apply_target_failure(
            &mut rolls,
            ComparePoint::GreaterThan(5.0),
            ComparePoint::LessThanOrEqual(2.0),
        );

        assert_eq!(rolls[0].value, 6);
        assert!(rolls[0].was_modifier_applied(ModifierFlags::TargetSuccess as u8))
    }

    #[test]
    fn test_modifier_target_failure_not_applied() {
        let mut rolls = [Roll::new(4)];
        apply_target_failure(
            &mut rolls,
            ComparePoint::LessThan(2.0),
            ComparePoint::GreaterThan(5.0),
        );

        assert_eq!(rolls[0].value, 4);
        assert!(!rolls[0].was_modifier_applied(ModifierFlags::TargetFailure as u8))
    }

    #[test]
    fn test_modifier_critical_success() {
        let mut rolls = [Roll::new(6)];
        apply_critical_success(&five_d6(vec![]), &mut rolls, None);

        assert_eq!(rolls[0].value, 6);
        assert!(rolls[0].was_modifier_applied(ModifierFlags::CriticalSuccess as u8))
    }

    #[test]
    fn test_modifier_critical_success_compare_point() {
        let mut rolls = [Roll::new(2)];
        apply_critical_success(&five_d6(vec![]), &mut rolls, Some(ComparePoint::Equal(2.0)));

        assert_eq!(rolls[0].value, 2);
        assert!(rolls[0].was_modifier_applied(ModifierFlags::CriticalSuccess as u8))
    }

    #[test]
    fn test_modifier_critical_success_not_applied() {
        let mut rolls = [Roll::new(2)];
        apply_critical_success(&five_d6(vec![]), &mut rolls, Some(ComparePoint::Equal(3.0)));

        assert_eq!(rolls[0].value, 2);
        assert!(!rolls[0].was_modifier_applied(ModifierFlags::CriticalSuccess as u8))
    }

    #[test]
    fn test_modifier_critical_failure() {
        let mut rolls = [Roll::new(1)];
        apply_critical_failure(&five_d6(vec![]), &mut rolls, None);

        assert_eq!(rolls[0].value, 1);
        assert!(rolls[0].was_modifier_applied(ModifierFlags::CriticalFailure as u8))
    }

    #[test]
    fn test_modifier_critical_failure_compare_point() {
        let mut rolls = [Roll::new(1)];
        apply_critical_failure(
            &five_d6(vec![]),
            &mut rolls,
            Some(ComparePoint::LessThanOrEqual(2.0)),
        );

        assert_eq!(rolls[0].value, 1);
        assert!(rolls[0].was_modifier_applied(ModifierFlags::CriticalFailure as u8))
    }

    #[test]
    fn test_modifier_critical_failure_not_applied() {
        let mut rolls = [Roll::new(4)];
        apply_critical_failure(
            &five_d6(vec![]),
            &mut rolls,
            Some(ComparePoint::GreaterThan(5.0)),
        );

        assert_eq!(rolls[0].value, 4);
        assert!(!rolls[0].was_modifier_applied(ModifierFlags::CriticalFailure as u8))
    }

    #[test]
    fn test_modifier_keep() {
        let mut rolls = [Roll::new(1), Roll::new(2), Roll::new(3), Roll::new(4)];
        apply_keep(&mut rolls, KeepKind::Highest, 2);

        assert_eq!(to_notations(&rolls), "[1d, 2d, 3, 4]");
    }

    #[test]
    fn test_modifier_keep_not_ordered() {
        let mut rolls = [Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)];

        apply_keep(&mut rolls, KeepKind::Highest, 2);

        assert_eq!(to_notations(&rolls), "[2d, 3, 4, 1d]");
    }

    #[test]
    fn test_modifier_keep_lowest_not_ordered() {
        let mut rolls = [Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)];

        apply_keep(&mut rolls, KeepKind::Lowest, 2);

        assert_eq!(to_notations(&rolls), "[2, 3d, 4d, 1]");
    }

    #[test]
    fn test_modifier_drop() {
        let mut rolls = [Roll::new(1), Roll::new(2), Roll::new(3), Roll::new(4)];

        apply_drop(&mut rolls, KeepKind::Highest, 3);

        assert_eq!(to_notations(&rolls), "[1, 2d, 3d, 4d]");
    }

    #[test]
    fn test_modifier_drop_not_ordered() {
        let mut rolls = [Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)];

        apply_drop(&mut rolls, KeepKind::Highest, 1);

        assert_eq!(to_notations(&rolls), "[2, 3, 4d, 1]");
    }

    #[test]
    fn test_modifier_drop_lowest_not_ordered() {
        let mut rolls = [Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)];

        apply_drop(&mut rolls, KeepKind::Lowest, 1);

        assert_eq!(to_notations(&rolls), "[2, 3, 4, 1d]");
    }

    #[test]
    fn test_modifier_sort_ascending() {
        let mut rolls = [Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)];

        apply_sort(&mut rolls, SortKind::Ascending);

        assert_eq!(values(&rolls), [1, 2, 3, 4]);
    }

    #[test]
    fn test_modifier_sort_descending() {
        let mut rolls = [Roll::new(2), Roll::new(3), Roll::new(4), Roll::new(1)];

        apply_sort(&mut rolls, SortKind::Descending);

        assert_eq!(values(&rolls), [4, 3, 2, 1]);
    }

    #[test]
    fn test_dice_can_pass_compare_point() {
        use ComparePoint::*;
        use DiceKind::*;

        let inputs = [
            // Equal
            (Standard(100), Equal(0.), false),
            (Standard(100), Equal(101.), false),
            (Standard(100), Equal(1.), true),
            (Standard(100), Equal(100.), true),
            // Not Equal
            (Standard(100), NotEqual(0.), true),
            (Standard(100), NotEqual(101.), true),
            (Standard(100), NotEqual(1.), false),
            (Standard(100), NotEqual(100.), false),
            // Less than
            (Standard(100), LessThan(1.), false),
            (Standard(100), LessThan(2.), true),
            (Standard(100), LessThan(101.), true),
            (Fudge1, LessThan(0.), true),
            (Fudge2, LessThan(0.), true),
            (Fudge1, LessThan(-1.), false),
            (Fudge2, LessThan(-1.), false),
            // Greater than
            (Standard(100), GreaterThan(1.), true),
            (Standard(100), GreaterThan(100.), false),
            (Standard(100), GreaterThan(101.), false),
            (Fudge1, GreaterThan(0.), true),
            (Fudge2, GreaterThan(0.), true),
            (Fudge1, GreaterThan(-1.), true),
            (Fudge2, GreaterThan(-1.), true),
            (Fudge1, GreaterThan(2.), false),
            (Fudge2, GreaterThan(2.), false),
            // Less than or equal
            (Standard(100), LessThanOrEqual(1.), true),
            (Standard(100), LessThanOrEqual(2.), true),
            (Standard(100), LessThanOrEqual(101.), true),
            (Fudge1, LessThanOrEqual(0.), true),
            (Fudge2, LessThanOrEqual(0.), true),
            (Fudge1, LessThanOrEqual(-1.), true),
            (Fudge2, LessThanOrEqual(-1.), true),
            // Greater than or equal
            (Standard(100), GreaterThanOrEqual(1.), true),
            (Standard(100), GreaterThanOrEqual(100.), true),
            (Standard(100), GreaterThanOrEqual(101.), false),
            (Fudge1, GreaterThanOrEqual(0.), true),
            (Fudge2, GreaterThanOrEqual(0.), true),
            (Fudge1, GreaterThanOrEqual(1.), true),
            (Fudge2, GreaterThanOrEqual(1.), true),
            (Fudge1, GreaterThanOrEqual(2.), false),
            (Fudge2, GreaterThanOrEqual(2.), false),
        ];

        for (dice_kind, compare_point, expected) in inputs {
            let dice = Dice::new(1, dice_kind, &[]);
            let result = compare_point.can_dice_pass(&dice);

            assert_eq!(
                result, expected,
                "{dice_kind:?} {compare_point:?} {expected}"
            );
        }
    }
}
