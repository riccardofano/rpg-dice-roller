use thiserror::Error;
use winnow::{
    ascii::digit0,
    combinator::{alt, empty, separated_pair},
    error::{
        StrContext::{Expected, Label},
        StrContextValue::{CharLiteral, Description, StringLiteral},
    },
    token::take_while,
    PResult, Parser,
};

#[derive(Debug, Error)]
pub enum DiceParseError {
    #[error("failed to parse")]
    ParseError,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DieKind {
    /// Standard dice with number of sides
    Standard(i32),
    /// Fudge/Fate die with 4 blanks, 1 plus, 1 minus
    Fudge1,
    /// Fudge/Fate die with 2 blanks, 2 plus, 2 minus
    Fudge2,
}

#[derive(Debug, Clone, Copy)]
pub struct Dice {
    quantity: i32,
    kind: DieKind,
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
    .map(|(quantity, kind)| Dice { quantity, kind })
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

fn non_zero_start_number(input: &mut &str) -> PResult<i32> {
    (take_while(1.., '1'..='9'), digit0)
        .parse_next(input)
        .map(|(non_zero, other)| format!("{non_zero}{other}").parse().unwrap_or(0))
}

#[cfg(test)]
mod tests {
    use crate::{Dice, DieKind};

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
}
