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
    parse_group_modifier, Expression, MathFn1, MathFn2, Modifier, Operator,
};

impl Expression {
    pub(crate) fn parse(input: &str) -> Result<Self, String> {
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
            |(expressions, modifiers): (Vec<Expression>, Vec<Modifier>)| {
                Expression::Group(expressions, Modifier::filter(&modifiers))
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
    use crate::parse::{KeepKind, MathFn2};

    use super::*;

    fn val(float: f64) -> Box<Expression> {
        Box::new(Expression::Value(float))
    }
    fn parens(expr: Box<Expression>) -> Box<Expression> {
        Box::new(Expression::Parens(expr))
    }
    fn infix(op: Operator, lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
        Box::new(Expression::Infix(op, lhs, rhs))
    }

    #[test]
    fn test_expression() {
        let input = "10d6 + 5d3s";
        let expression = Expression::parse(input).unwrap();

        assert_eq!(expression.to_string(), input);
    }

    #[test]
    fn test_infix_expressions() {
        #[rustfmt::skip]
        let inputs = [
            ( "1 + 2", Expression::Infix(Operator::Add, Box::new(Expression::Value(1.0)), Box::new(Expression::Value(2.0))) ),
            ("3 - 4", Expression::Infix(Operator::Sub, Box::new(Expression::Value(3.0)), Box::new(Expression::Value(4.0)))),
            ("4 * 5", Expression::Infix(Operator::Mul, Box::new(Expression::Value(4.0)), Box::new(Expression::Value(5.0)))),
            ("5 / 6", Expression::Infix(Operator::Div, Box::new(Expression::Value(5.0)), Box::new(Expression::Value(6.0)))),
            ("6 % 7", Expression::Infix(Operator::Rem, Box::new(Expression::Value(6.0)), Box::new(Expression::Value(7.0)))),
            ("7 ** 8", Expression::Infix(Operator::Pow, Box::new(Expression::Value(7.0)), Box::new(Expression::Value(8.0)))),
        ];

        for (input, expected) in inputs {
            let expression = Expression::parse(input).unwrap();
            assert_eq!(expression, expected);
            assert_eq!(expression.to_string(), input);
        }
    }

    #[test]
    fn test_infix_pow_variant_expression() {
        #[rustfmt::skip]
        let input = "2 ^ 6";

        let expression = Expression::parse(input).unwrap();
        assert_eq!(expression.to_string(), "2 ** 6");
        assert_eq!(
            expression,
            Expression::Infix(
                Operator::Pow,
                Box::new(Expression::Value(2.0)),
                Box::new(Expression::Value(6.0))
            )
        )
    }

    #[test]
    fn test_parens_expressions() {
        #[rustfmt::skip]
        let inputs = [
            ( "(1)", *parens(val(1.0))),
            ( "(1 + 1)", *parens(infix(Operator::Add, val(1.0), val(1.0)))),
        ];

        for (input, expected) in inputs {
            let expression = Expression::parse(input).unwrap();
            assert_eq!(expression, expected);
            assert_eq!(expression.to_string(), input);
        }
    }

    #[test]
    fn test_fn1_expressions() {
        #[rustfmt::skip]
        let inputs = [
            ( "abs(1)", Expression::Fn1(MathFn1::Abs, parens(val(1.0)))),
            ( "floor(1 / 5)", Expression::Fn1(MathFn1::Floor, parens(infix(Operator::Div, val(1.0), val(5.0))))),
            ( "ceil(2)", Expression::Fn1(MathFn1::Ceil, parens(val(2.0)))),
            ( "round(2)", Expression::Fn1(MathFn1::Round, parens(val(2.0)))),
            ( "sign(2)", Expression::Fn1(MathFn1::Sign, parens(val(2.0)))),
            ( "sqrt(2)", Expression::Fn1(MathFn1::Sqrt, parens(val(2.0)))),
            ( "log(2)", Expression::Fn1(MathFn1::Log, parens(val(2.0)))),
            ( "exp(2)", Expression::Fn1(MathFn1::Exp, parens(val(2.0)))),
            ( "sin(2)", Expression::Fn1(MathFn1::Sin, parens(val(2.0)))),
            ( "cos(2)", Expression::Fn1(MathFn1::Cos, parens(val(2.0)))),
            ( "tan(2)", Expression::Fn1(MathFn1::Tan, parens(val(2.0)))),
        ];

        for (input, expected) in inputs {
            let expression = Expression::parse(input).unwrap();
            assert_eq!(expression, expected);
            assert_eq!(expression.to_string(), input);
        }
    }

    #[test]
    fn test_fn2_expressions() {
        #[rustfmt::skip]
        let inputs = [
            ( "min(1, 5)", Expression::Fn2(MathFn2::Min, val(1.0), val(5.0))),
            ( "max(54, 60483)", Expression::Fn2(MathFn2::Max, val(54.0), val(60483.0))),
            ( "pow(48, 3)", Expression::Fn2(MathFn2::Pow, val(48.0), val(3.0))),
        ];

        for (input, expected) in inputs {
            let expression = Expression::parse(input).unwrap();
            assert_eq!(expression, expected);
            assert_eq!(expression.to_string(), input);
        }
    }

    #[test]
    fn test_group_expressions() {
        #[rustfmt::skip]
        let inputs = [
            ( "{1, 5}", Expression::Group(vec![*val(1.0), *val(5.0)], vec![])),
            ( "{1 % 3, 5}", Expression::Group(vec![*infix(Operator::Rem, val(1.0), val(3.0)), *val(5.0)], vec![])),
            ( "{1, 5}dl1", Expression::Group(vec![*val(1.0), *val(5.0)], vec![Modifier::Drop(KeepKind::Lowest, 1)])),
            ( "{1, 5}kh3dl1", Expression::Group(vec![*val(1.0), *val(5.0)], vec![Modifier::Keep(KeepKind::Highest, 3), Modifier::Drop(KeepKind::Lowest, 1)])),
        ];

        for (input, expected) in inputs {
            let expression = Expression::parse(input).unwrap();
            assert_eq!(expression, expected);
            assert_eq!(expression.to_string(), input);
        }
    }
}
