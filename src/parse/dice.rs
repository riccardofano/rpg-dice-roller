use winnow::{
    ascii::{dec_int, dec_uint, multispace0},
    combinator::{alt, cut_err, delimited, opt, preceded, repeat, separated_pair},
    PResult, Parser,
};

use super::{
    parse_fn1, parse_fn2, parse_parens, ComparePoint, Dice, DiceKind, ExplodingKind, Expression,
    KeepKind, Modifier, SortKind,
};

const TOTAL_MODIFIERS: usize = 12;

impl Modifier {
    pub fn discriminant(&self) -> u8 {
        // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u8` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        // NOTE: https://doc.rust-lang.org/std/mem/fn.discriminant.html#accessing-the-numeric-value-of-the-discriminant
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }

    pub fn join_all(modifiers: &[Modifier]) -> String {
        modifiers
            .iter()
            .map(|m| m.to_string())
            .collect::<Vec<_>>()
            .join("")
    }

    pub(crate) fn filter(modifiers: &[Modifier]) -> Vec<Modifier> {
        assert!(modifiers.len() < usize::MAX, "cmon, really?");

        let mut modifier_indices = [0; TOTAL_MODIFIERS];
        for (i, modifier) in modifiers.iter().enumerate() {
            modifier_indices[modifier.discriminant() as usize] = i + 1;
        }

        modifier_indices
            .into_iter()
            .filter(|&i| i > 0)
            .map(|i| modifiers[i - 1])
            .collect()
    }
}
impl PartialOrd for Modifier {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.discriminant().cmp(&other.discriminant()))
    }
}

impl Dice {
    /// Creates a new dice.
    /// The quantity will be set to 1 if 0 was passed in.
    /// The modifiers will be sorted in the order specified by the enum and only
    /// the last one of each variant will be applied.
    pub fn new(quantity: u32, kind: DiceKind, modifiers: &[Modifier]) -> Self {
        Self {
            quantity,
            kind,
            modifiers: Modifier::filter(modifiers),
        }
    }
}

pub fn parse_dice_standard(input: &mut &str) -> PResult<Expression> {
    separated_pair(
        opt(parse_dice_quantity),
        'd',
        cut_err((parse_dice_sides, repeat(0.., parse_modifier))),
    )
    .map(|(qty, (sides, modifiers))| {
        Expression::DiceStandard(qty.map(Box::new), Box::new(sides), modifiers)
    })
    .parse_next(input)
}

pub fn parse_dice_fudge1(input: &mut &str) -> PResult<Expression> {
    separated_pair(
        opt(parse_dice_quantity),
        "dF.1",
        cut_err(repeat(0.., parse_modifier)),
    )
    .map(|(qty, modifiers)| Expression::DiceFudge1(qty.map(Box::new), modifiers))
    .parse_next(input)
}

pub fn parse_dice_fudge2(input: &mut &str) -> PResult<Expression> {
    separated_pair(
        opt(parse_dice_quantity),
        alt(("dF.2", "dF")),
        cut_err(repeat(0.., parse_modifier)),
    )
    .map(|(qty, modifiers)| Expression::DiceFudge2(qty.map(Box::new), modifiers))
    .parse_next(input)
}

pub fn parse_dice_percentile(input: &mut &str) -> PResult<Expression> {
    separated_pair(
        opt(parse_dice_quantity),
        "d%",
        cut_err(repeat(0.., parse_modifier)),
    )
    .map(|(qty, modifiers)| Expression::DicePercentile(qty.map(Box::new), modifiers))
    .parse_next(input)
}

fn parse_dice_quantity(input: &mut &str) -> PResult<Expression> {
    delimited(
        multispace0,
        alt((
            parse_fn2,
            parse_fn1,
            parse_parens,
            dec_uint.map(|i: u32| Expression::Value(i as f64)),
        )),
        multispace0,
    )
    .parse_next(input)
}

fn parse_dice_sides(input: &mut &str) -> PResult<Expression> {
    delimited(
        multispace0,
        alt((
            parse_fn2,
            parse_fn1,
            parse_parens,
            dec_uint.map(|int: u32| Expression::Value(int as f64)),
        )),
        multispace0,
    )
    .parse_next(input)
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
        separated_pair(compare_point, 'f', cut_err(compare_point))
            .map(|(success, failure)| Modifier::TargetFailure(success, failure)),
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

pub fn parse_group_modifier(input: &mut &str) -> PResult<Modifier> {
    alt((
        preceded("kl", cut_err(dec_uint)).map(|n| Modifier::Keep(KeepKind::Lowest, n)),
        preceded("kh", cut_err(dec_uint)).map(|n| Modifier::Keep(KeepKind::Highest, n)),
        preceded('k', cut_err(dec_uint)).map(|n| Modifier::Keep(KeepKind::Highest, n)),
        preceded("dh", cut_err(dec_uint)).map(|n| Modifier::Drop(KeepKind::Highest, n)),
        preceded("dl", cut_err(dec_uint)).map(|n| Modifier::Drop(KeepKind::Lowest, n)),
        preceded('d', cut_err(dec_uint)).map(|n| Modifier::Drop(KeepKind::Lowest, n)),
        "sa".map(|_| Modifier::Sort(SortKind::Ascending)),
        "sd".map(|_| Modifier::Sort(SortKind::Descending)),
        's'.map(|_| Modifier::Sort(SortKind::Ascending)),
        separated_pair(compare_point, 'f', cut_err(compare_point))
            .map(|(success, failure)| Modifier::TargetFailure(success, failure)),
        compare_point.map(Modifier::TargetSuccess),
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

impl std::fmt::Display for Modifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Modifier::Min(val) => write!(f, "min{val}"),
            Modifier::Max(val) => write!(f, "max{val}"),
            Modifier::Exploding(kind, cmp) => write!(f, "{kind}{}", cmp_str(cmp)),
            Modifier::ReRoll(unique, cmp) => {
                write!(f, "r{}{}", if *unique { "o" } else { "" }, cmp_str(cmp))
            }
            Modifier::Unique(unique, cmp) => {
                write!(f, "u{}{}", if *unique { "o" } else { "" }, cmp_str(cmp))
            }
            Modifier::TargetSuccess(cmp) => write!(f, "{cmp}"),
            Modifier::TargetFailure(succ, fail) => write!(f, "{succ}f{fail}"),
            Modifier::CriticalSuccess(cmp) => write!(f, "cs{}", cmp_str(cmp)),
            Modifier::CriticalFailure(cmp) => write!(f, "cf{}", cmp_str(cmp)),
            Modifier::Keep(kind, amount) => write!(f, "k{kind}{amount}"),
            Modifier::Drop(kind, amount) => write!(f, "d{kind}{amount}"),
            Modifier::Sort(kind) => write!(f, "s{kind}"),
        }
    }
}
impl std::fmt::Display for ComparePoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparePoint::Equal(val) => write!(f, "={val}"),
            ComparePoint::NotEqual(val) => write!(f, "<>{val}"),
            ComparePoint::LessThan(val) => write!(f, "<{val}"),
            ComparePoint::GreaterThan(val) => write!(f, ">{val}"),
            ComparePoint::LessThanOrEqual(val) => write!(f, "<={val}"),
            ComparePoint::GreaterThanOrEqual(val) => write!(f, ">={val}"),
        }
    }
}
// Just because I don't want to keep typing it
fn cmp_str(cmp: &Option<ComparePoint>) -> String {
    cmp.map(|c| c.to_string()).unwrap_or_default()
}
impl std::fmt::Display for ExplodingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ExplodingKind::Standard => "!",
            ExplodingKind::Penetrating => "!p",
            ExplodingKind::Compounding => "!!",
            ExplodingKind::PenetratingCompounding => "!!p",
        };

        write!(f, "{str}")
    }
}
impl std::fmt::Display for KeepKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Always show the letter because it changes which one can be omitted
        // depending on if it's Drop or Keep
        let str = match self {
            KeepKind::Highest => "h",
            KeepKind::Lowest => "l",
        };

        write!(f, "{str}")
    }
}
impl std::fmt::Display for SortKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            SortKind::Ascending => "",
            SortKind::Descending => "d",
        };

        write!(f, "{str}")
    }
}

#[cfg(test)]
mod tests {
    use winnow::Parser;

    use crate::{
        parse::{ComparePoint, Modifier},
        Expression,
    };

    use super::{compare_point, parse_modifier, ExplodingKind, KeepKind, SortKind};

    /**
     * Parsing dice without modifiers
     */

    #[test]
    fn test_one_standard_d6() {
        let expression = Expression::parse("1d6").unwrap();

        let Expression::DiceStandard(qty, sides, mods) = expression else {
            panic!()
        };
        assert_eq!(*qty.unwrap(), Expression::Value(1.0));
        assert_eq!(*sides, Expression::Value(6.0));
        assert_eq!(mods, vec![]);
    }

    #[test]
    fn test_one_standard_d6_without_quantity() {
        let expression = Expression::parse("d6").unwrap();

        let Expression::DiceStandard(qty, sides, mods) = expression else {
            panic!()
        };
        assert!(qty.is_none());
        assert_eq!(*sides, Expression::Value(6.0));
        assert_eq!(mods, vec![]);
    }

    #[test]
    fn test_one_percentile_dice() {
        let expression = Expression::parse("1d%").unwrap();

        let Expression::DicePercentile(qty, mods) = expression else {
            panic!()
        };
        assert_eq!(*qty.unwrap(), Expression::Value(1.0));
        assert_eq!(mods, vec![]);
    }

    #[test]
    fn test_one_standard_fudge_die() {
        let expression = Expression::parse("1dF").unwrap();

        let Expression::DiceFudge2(qty, mods) = expression else {
            panic!()
        };
        assert_eq!(*qty.unwrap(), Expression::Value(1.0));
        assert_eq!(mods, vec![]);
    }

    #[test]
    fn test_one_standard_fudge_die_dot_notation() {
        let expression = Expression::parse("1dF.2").unwrap();

        let Expression::DiceFudge2(qty, mods) = expression else {
            panic!()
        };
        assert_eq!(*qty.unwrap(), Expression::Value(1.0));
        assert_eq!(mods, vec![]);
    }

    #[test]
    fn test_one_variant_fudge_die() {
        let expression = Expression::parse("1dF.1").unwrap();

        let Expression::DiceFudge1(qty, mods) = expression else {
            panic!()
        };
        assert_eq!(*qty.unwrap(), Expression::Value(1.0));
        assert_eq!(mods, vec![]);
    }

    /**
     * Parsing dice with modifiers
     */

    #[test]
    fn test_one_standard_d6_with_one_min_modifier() {
        let expression = Expression::parse("d6min3").unwrap();

        let Expression::DiceStandard(qty, sides, mods) = expression else {
            panic!()
        };
        assert!(qty.is_none());
        assert_eq!(*sides, Expression::Value(6.0));
        assert_eq!(mods, vec![Modifier::Min(3)]);
    }

    #[test]
    fn test_one_standard_d6_with_min_max_modifiers() {
        let expression = Expression::parse("d6max4min2").unwrap();

        let Expression::DiceStandard(qty, sides, mods) = expression else {
            panic!()
        };
        assert!(qty.is_none());
        assert_eq!(*sides, Expression::Value(6.0));
        // It's fine for the modifiers to not be sorted,
        // When I print the parsed expression they will be in the same order as the input
        // making it easy to test the parsed.to_string() to the input
        assert_eq!(mods, vec![Modifier::Max(4), Modifier::Min(2)]);
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
}
