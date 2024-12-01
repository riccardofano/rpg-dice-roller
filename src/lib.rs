//! Roll dice with modifiers and apply expressions to them.
//!
//! ```rust
//! # use rpg_dice_roller::{roll, roll_with, Dice, DiceKind, Modifier};
//! # use rand::rngs::StdRng;
//! # use rand::SeedableRng;
//! #
//! # fn main() -> Result<(), String> {
//! // Roll 3 d20, make the minimum value 5, keep the lowest 2 rolls.
//! let rolled = roll("3d20min5kl2")?;
//! println!("{rolled} = {}", rolled.value()); // [6, 5^, 17d] = 11
//!
//! // Use a custom Rng that implements the rand::Rng trait
//! let mut rng = StdRng::seed_from_u64(1);
//! let rolled = roll_with("3d200", &mut rng)?;
//! println!("{rolled} = {}", rolled.value()); // [165, 195, 160] = 520
//!
//! // Create Dice directly without parsing
//! let dice = Dice::new(5, DiceKind::Standard(8), &[Modifier::Min(5)]);
//! let rolled = dice.roll_all();
//! println!("{rolled} = {}", rolled.value()); // [8, 6, 5^, 8, 5] = 32
//!
//! let rolled = dice.roll_once();
//! println!("{rolled} = {}", rolled.value()); // [8] = 8
//!
//! # Ok(())
//! # }
//! ```

mod evaluate;
mod parse;

pub use evaluate::expression::RolledExpression;
pub use parse::{
    ComparePoint, Dice, DiceKind, ExplodingKind, Expression, KeepKind, MathFn1, MathFn2, Modifier,
};

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
