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
    /// Target failure must always be preceeded by a target success
    TargetFailure(ComparePoint, ComparePoint),
    CriticalSuccess(Option<ComparePoint>),
    CriticalFailure(Option<ComparePoint>),
    Keep(KeepKind, u32),
    Drop(KeepKind, u32),
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
