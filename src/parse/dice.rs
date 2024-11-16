use winnow::{
    ascii::{dec_int, dec_uint},
    combinator::{alt, cut_err, opt, preceded, repeat, separated_pair},
    error::{
        StrContext::{Expected, Label},
        StrContextValue::{CharLiteral, StringLiteral},
    },
    PResult, Parser,
};

use super::expression::parse_expr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiceKind {
    /// Standard dice with number of sides
    Standard(u32),
    /// Fudge/Fate die with 4 blanks, 1 plus, 1 minus
    Fudge1,
    /// Fudge/Fate die with 2 blanks, 2 plus, 2 minus
    Fudge2,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComparePoint {
    Equal(f64),
    NotEqual(f64),
    LessThan(f64),
    GreaterThan(f64),
    LessThanOrEqual(f64),
    GreaterThanOrEqual(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExplodingKind {
    Standard,
    Penetrating,
    Compounding,
    PenetratingCompounding,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortKind {
    Ascending,
    Descending,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeepKind {
    Highest,
    Lowest,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Modifier {
    Min(i32),
    Max(i32),
    Exploding(ExplodingKind, Option<ComparePoint>),
    /// True means to only re-roll once
    ReRoll(bool, Option<ComparePoint>),
    /// True means to only re-roll a unique dice once
    Unique(bool, Option<ComparePoint>),
    TargetSuccess(ComparePoint),
    TargetFailure(ComparePoint),
    CriticalSuccess(Option<ComparePoint>),
    CriticalFailure(Option<ComparePoint>),
    Keep(KeepKind, u32),
    Drop(KeepKind, u32),
    Sort(SortKind),
}

impl Modifier {
    pub fn discriminant(&self) -> u8 {
        // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u8` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        // NOTE: https://doc.rust-lang.org/std/mem/fn.discriminant.html#accessing-the-numeric-value-of-the-discriminant
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }
}
impl PartialOrd for Modifier {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.discriminant().cmp(&other.discriminant()))
    }
}

#[derive(Debug, Clone)]
pub struct Dice {
    pub(crate) quantity: u32,
    pub(crate) kind: DiceKind,
    pub(crate) modifiers: Vec<Modifier>,
}

impl Dice {
    pub fn parse(input: &str) -> Result<Dice, String> {
        parse_dice.parse(input).map_err(|e| e.to_string())
    }
}

pub fn parse_dice(input: &mut &str) -> PResult<Dice> {
    (
        separated_pair(opt(parse_dice_quantity), 'd', parse_dice_kind),
        repeat(0.., parse_modifier),
    )
        .map(
            |((quantity, kind), mut modifiers): ((Option<u32>, DiceKind), Vec<Modifier>)| {
                // Make sure modifiers are sorted by the order specified in the enum
                // TODO: Remove duplicates?
                modifiers.sort_by_key(|a| a.discriminant());

                Dice {
                    quantity: quantity.unwrap_or(1),
                    kind,
                    modifiers,
                }
            },
        )
        .context(Label("Dice"))
        .parse_next(input)
}

fn parse_dice_kind(input: &mut &str) -> PResult<DiceKind> {
    alt((
        '%'.map(|_| DiceKind::Standard(100))
            .context(Label("Percentile die"))
            .context(Expected(CharLiteral('%'))),
        "F.1"
            .map(|_| DiceKind::Fudge1)
            .context(Label("Fudge die variant"))
            .context(Expected(StringLiteral("F.1"))),
        "F.2"
            .map(|_| DiceKind::Fudge2)
            .context(Label("Standard Fudge die"))
            .context(Expected(StringLiteral("F.2"))),
        'F'.map(|_| DiceKind::Fudge2)
            .context(Label("Standard Fudge die"))
            .context(Expected(CharLiteral('F'))),
        dec_uint
            .map(|i: u32| DiceKind::Standard(i))
            .context(Label("Standard")),
        parse_expr.map(|e| DiceKind::Standard(e.evaluate() as u32)),
    ))
    .context(Label("Die kind"))
    .parse_next(input)
}

fn parse_dice_quantity(input: &mut &str) -> PResult<u32> {
    alt((dec_uint, parse_expr.map(|e| e.evaluate() as u32))).parse_next(input)
}

pub fn parse_modifier(input: &mut &str) -> PResult<Modifier> {
    alt((
        preceded("min", cut_err(dec_int)).map(Modifier::Min),
        preceded("max", cut_err(dec_int)).map(Modifier::Max),
        // TODO: Shouldn't cut_err because the ! could be a ComparePoint::NotEqual,
        // I might decide to not support != because it's confusing, it's standard syntax
        // in most common syntax languages but in this case you need to know how it
        // interacts with different modifiers
        preceded("!", cut_err(exploding)),
        preceded("ro", opt(compare_point)).map(|cp| Modifier::ReRoll(true, cp)),
        preceded("r", opt(compare_point)).map(|cp| Modifier::ReRoll(false, cp)),
        preceded("uo", opt(compare_point)).map(|cp| Modifier::Unique(true, cp)),
        preceded("u", opt(compare_point)).map(|cp| Modifier::Unique(false, cp)),
        preceded("kl", cut_err(dec_uint)).map(|n| Modifier::Keep(KeepKind::Lowest, n)),
        preceded("kh", cut_err(dec_uint)).map(|n| Modifier::Keep(KeepKind::Highest, n)),
        preceded('k', cut_err(dec_uint)).map(|n| Modifier::Keep(KeepKind::Highest, n)),
        preceded("dh", cut_err(dec_uint)).map(|n| Modifier::Drop(KeepKind::Highest, n)),
        preceded("dl", cut_err(dec_uint)).map(|n| Modifier::Drop(KeepKind::Lowest, n)),
        preceded('d', cut_err(dec_uint)).map(|n| Modifier::Drop(KeepKind::Lowest, n)),
        preceded("cs", opt(compare_point)).map(Modifier::CriticalSuccess),
        preceded("cf", opt(compare_point)).map(Modifier::CriticalFailure),
        "sa".map(|_| Modifier::Sort(SortKind::Ascending)),
        "sd".map(|_| Modifier::Sort(SortKind::Descending)),
        's'.map(|_| Modifier::Sort(SortKind::Ascending)),
        preceded('f', cut_err(compare_point)).map(Modifier::TargetFailure),
        compare_point.map(Modifier::TargetSuccess),
    ))
    .parse_next(input)
}

fn exploding(input: &mut &str) -> PResult<Modifier> {
    alt((
        preceded("!p", opt(compare_point))
            .map(|cp| Modifier::Exploding(ExplodingKind::PenetratingCompounding, cp)),
        preceded("p", opt(compare_point))
            .map(|cp| Modifier::Exploding(ExplodingKind::Penetrating, cp)),
        preceded("!", opt(compare_point))
            .map(|cp| Modifier::Exploding(ExplodingKind::Compounding, cp)),
        opt(compare_point).map(|cp| Modifier::Exploding(ExplodingKind::Standard, cp)),
    ))
    .parse_next(input)
}

fn compare_point(input: &mut &str) -> PResult<ComparePoint> {
    alt((
        preceded("<=", cut_err(dec_int)).map(|i: i32| ComparePoint::LessThanOrEqual(f64::from(i))),
        preceded(">=", cut_err(dec_int))
            .map(|i: i32| ComparePoint::GreaterThanOrEqual(f64::from(i))),
        // TODO: Missing != (not equal), it should work with every modifier
        // except the exploding ones, those need to use <>
        preceded("<>", cut_err(dec_int)).map(|i: i32| ComparePoint::NotEqual(f64::from(i))),
        preceded('=', cut_err(dec_int)).map(|i: i32| ComparePoint::Equal(f64::from(i))),
        preceded('<', cut_err(dec_int)).map(|i: i32| ComparePoint::LessThan(f64::from(i))),
        preceded('>', cut_err(dec_int)).map(|i: i32| ComparePoint::GreaterThan(f64::from(i))),
    ))
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use winnow::Parser;

    use crate::parse::{ComparePoint, Modifier};

    use super::{compare_point, parse_modifier, Dice, DiceKind, ExplodingKind, KeepKind, SortKind};

    /**
     * Parsing dice without modifiers
     */

    #[test]
    fn test_one_standard_d6() {
        let dice = Dice::parse("1d6").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Standard(6))
    }

    #[test]
    fn test_one_standard_d6_without_quantity() {
        let dice = Dice::parse("d6").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Standard(6))
    }

    #[test]
    fn test_one_percentile_dice() {
        let dice = Dice::parse("1d%").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Standard(100))
    }

    #[test]
    fn test_one_standard_fudge_die() {
        let dice = Dice::parse("1dF").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Fudge2)
    }

    #[test]
    fn test_one_standard_fudge_die_dot_notation() {
        let dice = Dice::parse("1dF.2").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Fudge2)
    }

    #[test]
    fn test_one_variant_fudge_die() {
        let dice = Dice::parse("1dF.1").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Fudge1)
    }

    /**
     * Parsing modifiers alone
     */

    #[test]
    fn test_modifier_min() {
        let res = parse_modifier.parse("min3").unwrap();
        assert_eq!(res, Modifier::Min(3))
    }

    #[test]
    fn test_modifier_min_missing_amount() {
        assert!(parse_modifier.parse("min").is_err())
    }

    #[test]
    fn test_modifier_max() {
        let res = parse_modifier.parse("max6").unwrap();
        assert_eq!(res, Modifier::Max(6))
    }

    #[test]
    fn test_modifier_max_missing_amount() {
        assert!(parse_modifier.parse("maxa").is_err())
    }

    #[test]
    fn test_modifier_exploding_standard() {
        let res = parse_modifier.parse("!").unwrap();
        assert_eq!(res, Modifier::Exploding(ExplodingKind::Standard, None))
    }

    #[test]
    fn test_modifier_exploding_standard_not_equal() {
        let res = parse_modifier.parse("!<>8").unwrap();
        let expected =
            Modifier::Exploding(ExplodingKind::Standard, Some(ComparePoint::NotEqual(8.0)));
        assert_eq!(res, expected)
    }

    #[test]
    fn test_modifier_penetrating_standard() {
        let res = parse_modifier.parse("!p").unwrap();
        assert_eq!(res, Modifier::Exploding(ExplodingKind::Penetrating, None))
    }

    #[test]
    fn test_modifier_penetrating_standard_less_than() {
        let res = parse_modifier.parse("!p<54").unwrap();
        let expected = Modifier::Exploding(
            ExplodingKind::Penetrating,
            Some(ComparePoint::LessThan(54.0)),
        );
        assert_eq!(res, expected)
    }

    #[test]
    fn test_modifier_compounding_standard() {
        let res = parse_modifier.parse("!!").unwrap();
        assert_eq!(res, Modifier::Exploding(ExplodingKind::Compounding, None))
    }

    #[test]
    fn test_modifier_compounding_standard_greater_than() {
        let res = parse_modifier.parse("!!>67").unwrap();
        let expected = Modifier::Exploding(
            ExplodingKind::Compounding,
            Some(ComparePoint::GreaterThan(67.0)),
        );
        assert_eq!(res, expected)
    }

    #[test]
    fn test_modifier_penetrating_compounding_standard() {
        let res = parse_modifier.parse("!!p").unwrap();
        assert_eq!(
            res,
            Modifier::Exploding(ExplodingKind::PenetratingCompounding, None)
        )
    }

    #[test]
    fn test_modifier_penetrating_compounding_standard_greater_or_equal() {
        let res = parse_modifier.parse("!!p>=7").unwrap();
        let expected = Modifier::Exploding(
            ExplodingKind::PenetratingCompounding,
            Some(ComparePoint::GreaterThanOrEqual(7.0)),
        );
        assert_eq!(res, expected)
    }

    #[test]
    fn test_modifier_reroll() {
        let res = parse_modifier.parse("r").unwrap();
        assert_eq!(res, Modifier::ReRoll(false, None))
    }

    #[test]
    fn test_modifier_reroll_equals() {
        let res = parse_modifier.parse("r=6").unwrap();
        assert_eq!(res, Modifier::ReRoll(false, Some(ComparePoint::Equal(6.0))))
    }

    #[test]
    fn test_modifier_reroll_once() {
        let res = parse_modifier.parse("ro").unwrap();
        assert_eq!(res, Modifier::ReRoll(true, None))
    }

    #[test]
    fn test_modifier_reroll_once_less_than() {
        let res = parse_modifier.parse("ro<3").unwrap();
        assert_eq!(
            res,
            Modifier::ReRoll(true, Some(ComparePoint::LessThan(3.0)))
        )
    }

    #[test]
    fn test_modifier_unique() {
        let res = parse_modifier.parse("u").unwrap();
        assert_eq!(res, Modifier::Unique(false, None))
    }

    #[test]
    fn test_modifier_unique_equals() {
        let res = parse_modifier.parse("u=5").unwrap();
        assert_eq!(res, Modifier::Unique(false, Some(ComparePoint::Equal(5.0))))
    }

    #[test]
    fn test_modifier_unique_once() {
        let res = parse_modifier.parse("uo").unwrap();
        assert_eq!(res, Modifier::Unique(true, None))
    }

    #[test]
    fn test_modifier_unique_once_greater() {
        let res = parse_modifier.parse("uo>6").unwrap();
        let expected = Modifier::Unique(true, Some(ComparePoint::GreaterThan(6.0)));
        assert_eq!(res, expected)
    }

    #[test]
    fn test_modifier_keep_default() {
        let res = parse_modifier.parse("k2").unwrap();
        assert_eq!(res, Modifier::Keep(KeepKind::Highest, 2))
    }

    #[test]
    fn test_modifier_keep_default_missing_amount() {
        assert!(parse_modifier.parse("k").is_err())
    }

    #[test]
    fn test_modifier_keep_highest() {
        let res = parse_modifier.parse("kh3").unwrap();
        assert_eq!(res, Modifier::Keep(KeepKind::Highest, 3))
    }

    #[test]
    fn test_modifier_keep_highest_missing_amount() {
        assert!(parse_modifier.parse("kh").is_err())
    }

    #[test]
    fn test_modifier_keep_lowest() {
        let res = parse_modifier.parse("kl4").unwrap();
        assert_eq!(res, Modifier::Keep(KeepKind::Lowest, 4))
    }

    #[test]
    fn test_modifier_keep_lowest_missing_amount() {
        assert!(parse_modifier.parse("kl").is_err())
    }

    #[test]
    fn test_modifier_drop_default() {
        let res = parse_modifier.parse("d2").unwrap();
        assert_eq!(res, Modifier::Drop(KeepKind::Lowest, 2))
    }

    #[test]
    fn test_modifier_drop_default_missing_amount() {
        assert!(parse_modifier.parse("d").is_err())
    }

    #[test]
    fn test_modifier_drop_highest() {
        let res = parse_modifier.parse("dh3").unwrap();
        assert_eq!(res, Modifier::Drop(KeepKind::Highest, 3))
    }

    #[test]
    fn test_modifier_drop_highest_missing_amount() {
        assert!(parse_modifier.parse("dh").is_err())
    }

    #[test]
    fn test_modifier_drop_lowest() {
        let res = parse_modifier.parse("dl4").unwrap();
        assert_eq!(res, Modifier::Drop(KeepKind::Lowest, 4))
    }

    #[test]
    fn test_modifier_drop_lowest_missing_amount() {
        assert!(parse_modifier.parse("dl").is_err())
    }

    #[test]
    fn test_modifier_critical_success() {
        let res = parse_modifier.parse("cs").unwrap();
        assert_eq!(res, Modifier::CriticalSuccess(None))
    }

    #[test]
    fn test_modifier_critical_success_less_than_or_equal() {
        let res = parse_modifier.parse("cs<=5").unwrap();
        let expected = Modifier::CriticalSuccess(Some(ComparePoint::LessThanOrEqual(5.0)));
        assert_eq!(res, expected)
    }

    #[test]
    fn test_modifier_critical_failure() {
        let res = parse_modifier.parse("cf").unwrap();
        assert_eq!(res, Modifier::CriticalFailure(None))
    }

    #[test]
    fn test_modifier_critical_failure_greater_than_or_equal() {
        let res = parse_modifier.parse("cf>=1").unwrap();
        let expected = Modifier::CriticalFailure(Some(ComparePoint::GreaterThanOrEqual(1.0)));
        assert_eq!(res, expected)
    }

    #[test]
    fn test_modifier_sort_default() {
        let res = parse_modifier.parse("s").unwrap();
        assert_eq!(res, Modifier::Sort(SortKind::Ascending))
    }

    #[test]
    fn test_modifier_sort_ascending() {
        let res = parse_modifier.parse("sa").unwrap();
        assert_eq!(res, Modifier::Sort(SortKind::Ascending))
    }

    #[test]
    fn test_modifier_sort_descending() {
        let res = parse_modifier.parse("sd").unwrap();
        assert_eq!(res, Modifier::Sort(SortKind::Descending))
    }

    /**
     * Parsing compare points alone
     */

    #[test]
    fn test_compare_point_equals() {
        let res = compare_point.parse("=3").unwrap();
        assert_eq!(res, ComparePoint::Equal(3.0))
    }

    #[test]
    fn test_compare_point_not_equals() {
        let res = compare_point.parse("<>69").unwrap();
        assert_eq!(res, ComparePoint::NotEqual(69.0))
    }

    #[test]
    fn test_compare_point_less_than() {
        let res = compare_point.parse("<123").unwrap();
        assert_eq!(res, ComparePoint::LessThan(123.0))
    }

    #[test]
    fn test_compare_point_greater_than() {
        let res = compare_point.parse(">123").unwrap();
        assert_eq!(res, ComparePoint::GreaterThan(123.0))
    }

    #[test]
    fn test_compare_point_less_than_or_equal() {
        let res = compare_point.parse("<=123").unwrap();
        assert_eq!(res, ComparePoint::LessThanOrEqual(123.0))
    }

    #[test]
    fn test_compare_point_greater_than_or_equal() {
        let res = compare_point.parse(">=456").unwrap();
        assert_eq!(res, ComparePoint::GreaterThanOrEqual(456.0))
    }

    /**
     * Parsing dice with modifiers
     */

    #[test]
    fn test_one_standard_d6_with_one_min_modifier() {
        let dice = Dice::parse("d6min3").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Standard(6));
        assert_eq!(dice.modifiers, vec![Modifier::Min(3)]);
    }

    #[test]
    fn test_one_standard_d6_with_two_min_modifier() {
        let dice = Dice::parse("d6min3min2").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Standard(6));
        assert_eq!(dice.modifiers, vec![Modifier::Min(3), Modifier::Min(2)]);
    }

    #[test]
    fn test_one_standard_d6_with_min_max_modifiers() {
        let dice = Dice::parse("d6max4min2").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DiceKind::Standard(6));
        assert_eq!(dice.modifiers, vec![Modifier::Min(2), Modifier::Max(4)]);
    }
}