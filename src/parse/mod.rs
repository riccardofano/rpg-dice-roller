mod dice;
mod expression;

use bumpalo::Bump;
pub use dice::*;
pub use expression::*;
use winnow::{Parser, Stateful};

pub type Stream<'i, 'bp> = Stateful<&'i str, &'bp Bump>;

#[derive(Debug, Default)]
pub struct NotationParser {
    pub(crate) bump: Bump,
}

impl NotationParser {
    pub fn new() -> Self {
        Self { bump: Bump::new() }
    }

    pub fn parse(&mut self, input: &str) -> Result<Expression, String> {
        let mut stream = Stream {
            input,
            state: &mut self.bump,
        };

        parse_expr
            .parse_next(&mut stream)
            .map_err(|e| e.to_string())
    }
}

#[derive(Debug, Clone)]
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

type BumpExpression<'bp> = Expression<'bp>;

// TODO: turn all vecs into slices
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'bp> {
    Value(f64),
    DiceStandard(
        Option<&'bp Expression<'bp>>,
        &'bp Expression<'bp>,
        Vec<Modifier>,
    ),
    DiceFudge1(Option<&'bp Expression<'bp>>, Vec<Modifier>),
    DiceFudge2(Option<&'bp Expression<'bp>>, Vec<Modifier>),
    DicePercentile(Option<&'bp Expression<'bp>>, Vec<Modifier>),
    Parens(&'bp Expression<'bp>),
    Group(Vec<Expression<'bp>>, Vec<Modifier>),
    Infix(Operator, &'bp Expression<'bp>, &'bp Expression<'bp>),
    Fn1(MathFn1, &'bp Expression<'bp>),
    Fn2(MathFn2, &'bp Expression<'bp>, &'bp Expression<'bp>),
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
