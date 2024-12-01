mod dice;
mod expression;

pub use dice::*;
pub use expression::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Dice {
    pub(crate) quantity: u32,
    pub(crate) kind: DiceKind,
    pub(crate) modifiers: Vec<Modifier>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiceKind {
    /// `d{sides}` or `d%` as a shortcut for `d100`\
    /// Die with specified number of sides.
    Standard(u32),
    /// `dF.1`\
    /// Fudge/Fate die with 4 blanks, 1 plus, 1 minus.
    Fudge1,
    /// `dF` or `dF.2`\
    /// Fudge/Fate die with 2 blanks, 2 plus, 2 minus.
    Fudge2,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComparePoint {
    /// =
    Equal(f64),
    /// <>
    NotEqual(f64),
    /// <
    LessThan(f64),
    /// \>
    GreaterThan(f64),
    /// <=
    LessThanOrEqual(f64),
    /// \>=
    GreaterThanOrEqual(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExplodingKind {
    /// `!` or `!{compare_point}`\
    /// Rolls an additional die.
    Standard,
    /// `!p` or `!p{compare_point}`\
    /// Rolls an additional die but reduces its value by 1.
    Penetrating,
    /// `!!` or `!!{compare_point}`\
    /// Rolls an additional die and adds its value to the previous roll.
    Compounding,
    /// `!!p` or `!!p{compare_point}`\
    /// Rolls an additional die, reduces its value by 1 and adds it to the previous roll.
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
/// Modifiers are special optional notations placed after a die or expression group.
///
/// Group expression can only use Keep/Drop, TargetSuccess/Failure, and Sort.
/// Modifiers are applied in the order of the list below even if they were written in a different order in the input.\
/// If more than one of the same modifier appears in the input, the last modifier is the one that will be applied.
pub enum Modifier {
    /// `min{amount}`\
    /// Update the roll value to `amount` if it was **below** it.
    Min(i32),
    /// `max{amount}`\
    /// Update the roll value to `amount` if it was **above** it.
    Max(i32),
    /// The roll explodes whenever the value passed the compare point or it's the highest value on the dice if not specified.\
    /// The roll can explode more than once.
    /// See [ExplodingKind](ExplodingKind) for the different variants of exploding available.
    Exploding(ExplodingKind, Option<ComparePoint>),
    /// `r` or `r{compare_point}` | `ro` or `ro{compare_point}` for reroll once\
    /// Rerolls the dice if it was the lowest number on the dice or it hit the compare point.
    // True means to only re-roll once
    ReRoll(bool, Option<ComparePoint>),
    /// `u` or `u{compare_point}` | `uo` or `uo{compare_point}` for reroll unique once\
    /// Rerolls the dice if the value was previously seen before.
    // True means to only re-roll a unique dice once
    Unique(bool, Option<ComparePoint>),
    /// `{compare_point}`\
    /// The final roll value sum will be now determined by the amount of rolls that passed the compare point, +1 for every roll.
    TargetSuccess(ComparePoint),
    /// `{success_compare_point}f{failure_compare_point}`\
    /// The final roll value sum will be now determined by the amount of rolls that passed or failed the compare points, +1 for every success roll, -1 for every fail roll.
    TargetFailure(ComparePoint, ComparePoint),
    /// `cs` or `cs{compare_point}`\
    /// Purely cosmetic, adds the `**` notation if the roll was the highest value on the dice or passed the compare point.
    CriticalSuccess(Option<ComparePoint>),
    /// `cf` or `cf{compare_point}`\
    /// Purely cosmetic, adds the `__` notation if the roll was the lowest value on the dice or hit the compare point.
    CriticalFailure(Option<ComparePoint>),
    /// `k{amount}`, `kh{amount}` or `kl{amount}`\
    /// Drops every roll except the highest or lowest `{amount}`.
    /// (Defaults to keep highest).
    Keep(KeepKind, u32),
    /// `d{amount}`, `dh{amount}` or `dl{amount}`\
    /// Drops `{amount}` of lowest or highest rolls.
    /// (Defaults to drop lowest).
    Drop(KeepKind, u32),
    /// `s`, `sa` or `sd`\
    /// Sorts the roll values in ascending or descending order.
    /// (Defaults to sort ascending).
    Sort(SortKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Value(f64),
    DiceStandard(Option<Box<Expression>>, Box<Expression>, Vec<Modifier>),
    DiceFudge1(Option<Box<Expression>>, Vec<Modifier>),
    DiceFudge2(Option<Box<Expression>>, Vec<Modifier>),
    DicePercentile(Option<Box<Expression>>, Vec<Modifier>),
    Parens(Box<Expression>),
    Group(Vec<Expression>, Vec<Modifier>),
    Infix(Operator, Box<Expression>, Box<Expression>),
    Fn1(MathFn1, Box<Expression>),
    Fn2(MathFn2, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathFn1 {
    Abs,
    Floor,
    Ceil,
    Round,
    Sign,
    Sqrt,
    Log,
    Exp,
    Sin,
    Cos,
    Tan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathFn2 {
    Min,
    Max,
    Pow,
}
