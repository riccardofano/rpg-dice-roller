use winnow::{
    ascii::{dec_uint, multispace0},
    combinator::{
        alt, cut_err, delimited, dispatch, empty, fail, not, opt, peek, preceded, repeat,
        separated_pair,
    },
    token::{any, one_of},
    PResult, Parser,
};

use super::{parse_dice, parse_modifier, Dice, Modifier};

#[derive(Debug, Clone)]
pub enum Expression {
    Value(f64),
    Dice(Box<Expression>, Box<Expression>, Vec<Modifier>),
    Parens(Box<Expression>),
    Group(Box<Expression>),
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
            parse_dice_new,
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

fn parse_parens(input: &mut &str) -> PResult<Expression> {
    delimited('(', parse_expr, ')')
        .map(|e| Expression::Parens(Box::new(e)))
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

fn parse_dice_new(input: &mut &str) -> PResult<Expression> {
    separated_pair(
        parse_factor_no_dice,
        'd',
        (parse_factor, repeat(0.., parse_modifier)),
    )
    .map(|(qty, (val, mods))| Expression::Dice(Box::new(qty), Box::new(val), mods))
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expression() {
        let input = "(1d6 * 2d10) + 5";

        let result = Expression::parse(input);
        eprintln!("{:?}", result);

        todo!()
    }
}
