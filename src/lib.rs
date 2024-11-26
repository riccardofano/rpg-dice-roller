mod evaluate;
mod parse;

pub use evaluate::expression::RolledExpression;
pub use parse::{ComparePoint, Dice, DiceKind, Expression, KeepKind, MathFn1, MathFn2, Modifier};

/// Parses the notation returning the parsed abstract syntax tree without
/// rolling the dice.
pub fn parse(notation: &str) -> Result<Expression, String> {
    Expression::parse(notation)
}

/// Parses the notation returning the result of rolling all the dice parsed.
pub fn roll(notation: &str) -> Result<RolledExpression, String> {
    let expression = Expression::parse(notation)?;
    Ok(expression.roll(&mut rand::thread_rng()))
}

/// Same as `roll()` but allows you to choose the rng you prefer to use.
pub fn roll_with(notation: &str, rng: &mut impl rand::Rng) -> Result<RolledExpression, String> {
    let expression = Expression::parse(notation)?;
    Ok(expression.roll(rng))
}

#[cfg(test)]
mod tests {
    use rand::{rngs::StdRng, SeedableRng};

    use super::*;

    #[test]
    fn test_running() {
        let input = "dF=1";

        // let expression = parse(input).unwrap();

        // todo!()
    }
}
