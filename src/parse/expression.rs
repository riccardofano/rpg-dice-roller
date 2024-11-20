use winnow::{
    ascii::{dec_uint, multispace0},
    combinator::{
        alt, cut_err, delimited, dispatch, empty, fail, repeat, separated, separated_pair,
    },
    token::any,
    PResult, Parser,
};

use super::{
    parse_dice_fudge1, parse_dice_fudge2, parse_dice_percentile, parse_dice_standard,
    parse_group_modifier, Modifier,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Value(f64),
    DiceStandard(Option<Box<Expression>>, Box<Expression>, Vec<Modifier>),
    DiceFudge1(Option<Box<Expression>>, Vec<Modifier>),
    DiceFudge2(Option<Box<Expression>>, Vec<Modifier>),
    DicePercentile(Option<Box<Expression>>, Vec<Modifier>),
    Parens(Box<Expression>),
    // TODO: Groups should be vecs of expressions because you should be allowed to do
    // math on a dice and that dice should get sorted on the results of the
    // expression, not only the total value of the rolls
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
            parse_dice_fudge1,
            parse_dice_fudge2,
            parse_dice_percentile,
            parse_dice_standard,
            parse_fn2,
            parse_fn1,
            parse_roll_groups,
            parse_parens,
            dec_uint.map(|i: u32| Expression::Value(i as f64)),
        )),
        multispace0,
    )
    .parse_next(input)
}

pub fn parse_parens(input: &mut &str) -> PResult<Expression> {
    delimited('(', parse_expr, ')')
        .map(|e| Expression::Parens(Box::new(e)))
        .parse_next(input)
}

fn parse_roll_groups(input: &mut &str) -> PResult<Expression> {
    (
        delimited('{', separated(1.., parse_expr, ','), '}'),
        repeat(0.., parse_group_modifier),
    )
        .map(
            |(expressions, mut modifiers): (Vec<Expression>, Vec<Modifier>)| {
                modifiers.sort_by_key(|m| m.discriminant());
                Expression::Group(expressions, modifiers)
            },
        )
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

pub fn parse_fn1(input: &mut &str) -> PResult<Expression> {
    (parse_fn1_name, cut_err(parse_parens))
        .map(|(f, arg)| Expression::Fn1(f, Box::new(arg)))
        .parse_next(input)
}

pub fn parse_fn2(input: &mut &str) -> PResult<Expression> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expression() {
        let input = "10d6 + 5d3s";
        let expression = Expression::parse(input).unwrap();

        assert_eq!(expression.to_string(), input);
    }
}
