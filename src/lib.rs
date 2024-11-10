use thiserror::Error;
use winnow::{
    ascii::digit0,
    combinator::{alt, empty, preceded, separated_pair},
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
    /// Fate or Fudge die with number of - sides
    /// F.2 -> Fate.Sides
    Fate(i32),
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
        alt((non_zero_start_number, empty.map(|_| 1))),
        'd',
        die_kind,
    )
    .map(|(quantity, kind)| Dice { quantity, kind })
    .parse_next(input)
}

fn die_kind(input: &mut &str) -> PResult<DieKind> {
    alt((
        '%'.map(|_| DieKind::Standard(100)),
        alt((preceded("F.", non_zero_start_number), 'F'.map(|_| 2))).map(DieKind::Fate),
        non_zero_start_number.map(DieKind::Standard),
    ))
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
    fn test_one_default_fate_dice() {
        let dice = Dice::parse("1dF").unwrap();
        assert_eq!(dice.quantity, 1);
        assert_eq!(dice.kind, DieKind::Fate(2))
    }
}
