use winnow::{
    ascii::{dec_uint, multispace0},
    combinator::{
        alt, cut_err, delimited, dispatch, empty, fail, opt, repeat, separated, separated_pair,
    },
    token::any,
    PResult, Parser,
};

use super::{parse_dice_kind, parse_modifier, Dice, DiceKind, Modifier};
use crate::evaluate::{group_rolls::apply_group_modifiers, roll::RollOutput};

#[derive(Debug, Clone)]
pub enum Expression {
    Value(f64),
    DiceRolls(RollOutput),
    Parens(Box<Expression>),
    Group(Vec<RollOutput>),
    Infix(Operator, Box<Expression>, Box<Expression>),
    Fn1(MathFn1, Box<Expression>),
    Fn2(MathFn2, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum MathFn2 {
    Min,
    Max,
    Pow,
}

impl Expression {
    pub fn parse(input: &str) -> Result<Self, String> {
        parse_expr.parse(input).map_err(|e| e.to_string())
    }
}

pub fn parse_expr(input: &mut &str) -> PResult<Expression> {
    let init = parse_term.parse_next(input)?;

    repeat(0.., (low_precendence_operator, parse_term))
        .fold(
            move || init.clone(),
            |acc, (op, val): (Operator, Expression)| {
                Expression::Infix(op, Box::new(acc), Box::new(val))
            },
        )
        .parse_next(input)
}

fn parse_term(input: &mut &str) -> PResult<Expression> {
    let init = parse_factor(input)?;

    repeat(0.., (high_precendence_operator, parse_factor))
        .fold(
            move || init.clone(),
            |acc, (op, val): (Operator, Expression)| {
                Expression::Infix(op, Box::new(acc), Box::new(val))
            },
        )
        .parse_next(input)
}

fn parse_factor(input: &mut &str) -> PResult<Expression> {
    delimited(
        multispace0,
        alt((
            parse_dice.map(|d| Expression::DiceRolls(d.roll_all(rand::thread_rng()))),
            parse_roll_groups,
            parse_fn2,
            parse_fn1,
            parse_parens,
            dec_uint.map(|i: u32| Expression::Value(i as f64)),
        )),
        multispace0,
    )
    .parse_next(input)
}

fn parse_factor_no_dice(input: &mut &str) -> PResult<Expression> {
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

fn parse_factor_dice_kind(input: &mut &str) -> PResult<DiceKind> {
    delimited(
        multispace0,
        alt((
            parse_dice_kind,
            parse_fn2.map(|f| DiceKind::Standard(f.evaluate().round() as u32)),
            parse_fn1.map(|f| DiceKind::Standard(f.evaluate().round() as u32)),
            parse_parens.map(|expr| DiceKind::Standard(expr.evaluate().round() as u32)),
        )),
        multispace0,
    )
    .parse_next(input)
}

fn parse_parens(input: &mut &str) -> PResult<Expression> {
    delimited('(', parse_expr, ')')
        .map(|e| Expression::Parens(Box::new(e)))
        .parse_next(input)
}

fn parse_roll_groups(input: &mut &str) -> PResult<Expression> {
    (
        delimited('{', separated(1.., parse_dice, ','), '}'),
        repeat(0.., parse_modifier),
    )
        .map(|(dices, modifiers): (Vec<Dice>, Vec<Modifier>)| {
            let mut rolls = dices
                .into_iter()
                .map(|d| d.roll_all(rand::thread_rng()))
                .collect::<Vec<_>>();

            apply_group_modifiers(&mut rolls, &modifiers);
            Expression::Group(rolls)
        })
        .parse_next(input)
}

fn low_precendence_operator(input: &mut &str) -> PResult<Operator> {
    dispatch!(any;
        '+' => empty.value(Operator::Add),
        '-' => empty.value(Operator::Sub),
        _ => fail
    )
    .parse_next(input)
}

fn high_precendence_operator(input: &mut &str) -> PResult<Operator> {
    dispatch!(any;
        '*' => alt(('*'.value(Operator::Pow), empty.value(Operator::Mul))),
        '/' => empty.value(Operator::Div),
        '%' => empty.value(Operator::Rem),
        '^' => empty.value(Operator::Pow),
        _ => fail
    )
    .parse_next(input)
}

fn parse_fn1_name(input: &mut &str) -> PResult<MathFn1> {
    alt((
        "abs".value(MathFn1::Abs),
        "floor".value(MathFn1::Floor),
        "ceil".value(MathFn1::Ceil),
        "round".value(MathFn1::Round),
        "sign".value(MathFn1::Sign),
        "sqrt".value(MathFn1::Sqrt),
        "log".value(MathFn1::Log),
        "ln".value(MathFn1::Log),
        "exp".value(MathFn1::Exp),
        "sin".value(MathFn1::Sin),
        "cos".value(MathFn1::Cos),
        "tan".value(MathFn1::Tan),
    ))
    .parse_next(input)
}

fn parse_fn2_name(input: &mut &str) -> PResult<MathFn2> {
    alt((
        "min".value(MathFn2::Min),
        "max".value(MathFn2::Max),
        "pow".value(MathFn2::Pow),
    ))
    .parse_next(input)
}

fn parse_fn1(input: &mut &str) -> PResult<Expression> {
    (parse_fn1_name, cut_err(parse_parens))
        .map(|(f, arg)| Expression::Fn1(f, Box::new(arg)))
        .parse_next(input)
}

fn parse_fn2(input: &mut &str) -> PResult<Expression> {
    (
        parse_fn2_name,
        cut_err(delimited(
            '(',
            separated_pair(parse_expr, ',', parse_expr),
            ')',
        )),
    )
        .map(|(f, (arg1, arg2))| Expression::Fn2(f, Box::new(arg1), Box::new(arg2)))
        .parse_next(input)
}

pub fn parse_dice(input: &mut &str) -> PResult<Dice> {
    separated_pair(
        opt(parse_factor_no_dice),
        'd',
        (parse_factor_dice_kind, repeat(0.., parse_modifier)),
    )
    .map(|(qty, (kind, modifiers))| {
        let qty = qty.map(|q| q.evaluate().round() as u32).unwrap_or(1);
        Dice::new(qty, kind, modifiers)
    })
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expression() {
        let input = "{10d6, 5d3}s";
        let expression = Expression::parse(input).unwrap();

        dbg!(expression);
        // todo!()
    }
}
