use winnow::{
    ascii::digit0,
    combinator::{alt, cut_err, empty, fail, opt, preceded, separated_pair},
    error::{
        StrContext::{Expected, Label},
        StrContextValue::{CharLiteral, Description, StringLiteral},
    },
    token::take_while,
    PResult, Parser,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DieKind {
    /// Standard dice with number of sides
    Standard(u32),
    /// Fudge/Fate die with 4 blanks, 1 plus, 1 minus
    Fudge1,
    /// Fudge/Fate die with 2 blanks, 2 plus, 2 minus
    Fudge2,
}

#[derive(Debug, Clone, Copy)]
pub enum ComparePoint {
    Equal(u32),
    NotEqual(u32),
    LessThan(u32),
    GreaterThan(u32),
    LessThanOrEqual(u32),
    GreaterThanOrEqual(u32),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Modifier {
    Min(u32),
    Max(u32),
    Exploding(ExplodingKind, Option<ComparePoint>),
    /// True means to only re-roll once
    ReRoll(bool, Option<ComparePoint>),
    /// True means to only re-roll a unique dice once
    Unique(bool, Option<ComparePoint>),
    Keep(KeepKind, u32),
    Drop(KeepKind, u32),
    TargetSuccess(ComparePoint),
    TargetFailure(ComparePoint),
    CriticalSuccess(Option<ComparePoint>),
    CriticalFailure(Option<ComparePoint>),
    Sort(SortKind),
}

#[derive(Debug, Clone)]
pub struct Dice {
    pub(crate) quantity: u32,
    pub(crate) kind: DieKind,
    pub(crate) modifiers: Vec<Modifier>,
}

impl Dice {
    pub fn parse(input: &str) -> Result<Dice, String> {
        dice.parse(input).map_err(|e| e.to_string())
    }
}

fn dice(input: &mut &str) -> PResult<Dice> {
    separated_pair(
        alt((non_zero_start_number, empty.map(|_| 1)))
            .context(Label("dice quantity"))
            .context(Expected(Description(
                "quantity must either be a number without leading 0s or empty (to indicate 1 die)",
            ))),
        'd'.context(Label("d")).context(Expected(CharLiteral('d'))),
        die_kind,
    )
    .map(|(quantity, kind)| Dice {
        quantity,
        kind,
        modifiers: Vec::new(),
    })
    .context(Label("Dice"))
    .parse_next(input)
}

fn die_kind(input: &mut &str) -> PResult<DieKind> {
    alt((
        '%'.map(|_| DieKind::Standard(100))
            .context(Label("Percentile die"))
            .context(Expected(CharLiteral('%'))),
        "F.1"
            .map(|_| DieKind::Fudge1)
            .context(Label("Fudge die variant"))
            .context(Expected(StringLiteral("F.1"))),
        "F.2"
            .map(|_| DieKind::Fudge2)
            .context(Label("Standard Fudge die"))
            .context(Expected(StringLiteral("F.2"))),
        'F'.map(|_| DieKind::Fudge2)
            .context(Label("Standard Fudge die"))
            .context(Expected(CharLiteral('F'))),
        non_zero_start_number
            .map(DieKind::Standard)
            .context(Label("Standard")),
    ))
    .context(Label("Die kind"))
    .parse_next(input)
}

fn non_zero_start_number(input: &mut &str) -> PResult<u32> {
    (take_while(1.., '1'..='9'), digit0)
        .parse_next(input)
        .map(|(non_zero, other)| format!("{non_zero}{other}").parse().unwrap_or(0))
}

fn modifier(input: &mut &str) -> PResult<Modifier> {
    alt((
        preceded("min", cut_err(non_zero_start_number)).map(Modifier::Min),
        preceded("max", cut_err(non_zero_start_number)).map(Modifier::Max),
        preceded("!", cut_err(exploding)),
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

// TODO
fn compare_point(input: &mut &str) -> PResult<ComparePoint> {
    fail.parse_next(input)
}

#[cfg(test)]
mod tests {
    use winnow::Parser;

    use crate::parse::Modifier;

    use super::{modifier, Dice, DieKind, ExplodingKind};

    #[test]
    fn test_one_standard_d6() {
        let dice = Dice::parse("1d6").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DieKind::Standard(6))
    }

    #[test]
    fn test_one_standard_d6_without_quantity() {
        let dice = Dice::parse("d6").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DieKind::Standard(6))
    }

    #[test]
    fn test_one_percentile_dice() {
        let dice = Dice::parse("1d%").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DieKind::Standard(100))
    }

    #[test]
    fn test_one_standard_fudge_die() {
        let dice = Dice::parse("1dF").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DieKind::Fudge2)
    }

    #[test]
    fn test_one_standard_fudge_die_dot_notation() {
        let dice = Dice::parse("1dF.2").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DieKind::Fudge2)
    }

    #[test]
    fn test_one_variant_fudge_die() {
        let dice = Dice::parse("1dF.1").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DieKind::Fudge1)
    }

    #[test]
    fn test_modifier_min() {
        let res = modifier.parse("min3").unwrap();
        assert_eq!(res, Modifier::Min(3))
    }

    #[test]
    fn test_modifier_min_missing_amount() {
        assert!(modifier.parse("min").is_err())
    }

    #[test]
    fn test_modifier_max() {
        let res = modifier.parse("max6").unwrap();
        assert_eq!(res, Modifier::Max(6))
    }

    #[test]
    fn test_modifier_max_missing_amount() {
        assert!(modifier.parse("maxa").is_err())
    }

    #[test]
    fn test_modifier_exploding_standard() {
        let res = modifier.parse("!").unwrap();
        assert_eq!(res, Modifier::Exploding(ExplodingKind::Standard, None))
    }

    #[test]
    fn test_modifier_penetrating_standard() {
        let res = modifier.parse("!p").unwrap();
        assert_eq!(res, Modifier::Exploding(ExplodingKind::Penetrating, None))
    }

    #[test]
    fn test_modifier_compounding_standard() {
        let res = modifier.parse("!!").unwrap();
        assert_eq!(res, Modifier::Exploding(ExplodingKind::Compounding, None))
    }

    #[test]
    fn test_modifier_penetrating_compounding_standard() {
        let res = modifier.parse("!!p").unwrap();
        assert_eq!(
            res,
            Modifier::Exploding(ExplodingKind::PenetratingCompounding, None)
        )
    }
}