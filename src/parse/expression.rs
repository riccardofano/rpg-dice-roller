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
    parse_group_modifier, BumpExpression, Expression, MathFn1, MathFn2, Modifier, Operator, Stream,
};

pub fn parse_expr<'bp>(stream: &mut Stream<'_, 'bp>) -> PResult<BumpExpression<'bp>> {
    let init = parse_term.parse_next(stream)?;

    repeat(0.., (low_precendence_operator, parse_term))
        .fold(
            move || init.clone(),
            |acc, (op, val): (Operator, Expression)| {
                let acc_ref = stream.state.alloc(acc);
                let val = stream.state.alloc(val);
                Expression::Infix(op, acc_ref, val)
            },
        )
        .parse_next(stream)
}

fn parse_term<'bp>(stream: &mut Stream<'_, 'bp>) -> PResult<BumpExpression<'bp>> {
    let init = parse_factor(stream)?;

    repeat(0.., (high_precendence_operator, parse_term))
        .fold(
            move || init.clone(),
            |acc, (op, val): (Operator, Expression)| {
                let acc_ref = stream.state.alloc(acc);
                let val = stream.state.alloc(val);
                Expression::Infix(op, acc_ref, val)
            },
        )
        .parse_next(stream)
}

fn parse_factor<'bp>(stream: &mut Stream<'_, 'bp>) -> PResult<BumpExpression<'bp>> {
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
            uint_expr,
        )),
        multispace0,
    )
    .parse_next(stream)
}

pub fn parse_parens<'bp>(stream: &mut Stream<'_, 'bp>) -> PResult<BumpExpression<'bp>> {
    let mut parser = delimited('(', parse_expr, ')');
    let expression = stream.state.alloc(parser.parse_next(stream)?);

    Ok(Expression::Parens(expression))
}

pub fn uint_expr<'bp>(stream: &mut Stream<'_, 'bp>) -> PResult<BumpExpression<'bp>> {
    let uint: u32 = dec_uint.parse_next(stream)?;
    Ok(Expression::Value(uint as f64))
}

fn parse_roll_groups<'bp>(stream: &mut Stream<'_, 'bp>) -> PResult<BumpExpression<'bp>> {
    let mut parser = (
        delimited('{', separated(1.., parse_expr, ','), '}'),
        repeat(0.., parse_group_modifier),
    );

    let (expressions, mut modifiers): (Vec<_>, Vec<Modifier>) = parser.parse_next(stream)?;
    modifiers.sort_by_key(|m| m.discriminant());

    Ok(Expression::Group(expressions, modifiers))
}

fn low_precendence_operator(stream: &mut Stream<'_, '_>) -> PResult<Operator> {
    dispatch!(any;
        '+' => empty.value(Operator::Add),
        '-' => empty.value(Operator::Sub),
        _ => fail
    )
    .parse_next(stream)
}

fn high_precendence_operator(stream: &mut Stream<'_, '_>) -> PResult<Operator> {
    dispatch!(any;
        '*' => alt(('*'.value(Operator::Pow), empty.value(Operator::Mul))),
        '/' => empty.value(Operator::Div),
        '%' => empty.value(Operator::Rem),
        '^' => empty.value(Operator::Pow),
        _ => fail
    )
    .parse_next(stream)
}

fn parse_fn1_name(stream: &mut Stream<'_, '_>) -> PResult<MathFn1> {
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
    .parse_next(stream)
}

fn parse_fn2_name(stream: &mut Stream<'_, '_>) -> PResult<MathFn2> {
    alt((
        "min".value(MathFn2::Min),
        "max".value(MathFn2::Max),
        "pow".value(MathFn2::Pow),
    ))
    .parse_next(stream)
}

pub fn parse_fn1<'bp>(stream: &mut Stream<'_, 'bp>) -> PResult<BumpExpression<'bp>> {
    let mut parser = (parse_fn1_name, cut_err(parse_parens));
    let (f, arg) = parser.parse_next(stream)?;

    let arg = stream.state.alloc(arg);
    Ok(Expression::Fn1(f, arg))
}

pub fn parse_fn2<'bp>(stream: &mut Stream<'_, 'bp>) -> PResult<BumpExpression<'bp>> {
    let mut parser = (
        parse_fn2_name,
        cut_err(delimited(
            '(',
            separated_pair(parse_expr, ',', parse_expr),
            ')',
        )),
    );

    let (f, (arg1, arg2)) = parser.parse_next(stream)?;
    let arg1 = stream.state.alloc(arg1);
    let arg2 = stream.state.alloc(arg2);

    Ok(Expression::Fn2(f, arg1, arg2))
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use crate::NotationParser;

    use super::*;

    fn val(float: f64) -> &'static Expression<'static> {
        Box::leak(Box::new(Expression::Value(float)))
    }
    fn parens(expr: &'static Expression<'static>) -> &'static Expression<'static> {
        Box::leak(Box::new(Expression::Parens(expr)))
    }
    fn infix(
        op: Operator,
        lhs: &'static Expression<'static>,
        rhs: &'static Expression<'static>,
    ) -> &'static Expression<'static> {
        Box::leak(Box::new(Expression::Infix(op, lhs, rhs)))
    }

    #[test]
    fn test_expression() {
        let input = "10d6 + 5d3s";
        let mut parser = NotationParser::new();
        let expression = parser.parse(input).unwrap();

        assert_eq!(expression.to_string(), input);
    }

    #[test]
    fn test_infix_expressions() {
        let bump = Bump::new();

        #[rustfmt::skip]
        let inputs = [
            ( "1 + 2", Expression::Infix(Operator::Add, val(1.0), val(2.0))),
            ("3 - 4", Expression::Infix(Operator::Sub, val(3.0), val(4.0))),
            ("4 * 5", Expression::Infix(Operator::Mul, val(4.0), val(5.0))),
            ("5 / 6", Expression::Infix(Operator::Div, val(5.0), val(6.0))),
            ("6 % 7", Expression::Infix(Operator::Rem, val(6.0), val(7.0))),
            ("7 ** 8", Expression::Infix(Operator::Pow, val(7.0), val(8.0))),
        ];

        for (input, expected) in inputs {
            let mut parser = NotationParser::new();
            let expression = parser.parse(input).unwrap();
            assert_eq!(expression, expected);
            assert_eq!(expression.to_string(), input);
        }
    }

    #[test]
    fn test_infix_pow_variant_expression() {
        #[rustfmt::skip]
        let input = "2 ^ 6";

        let mut parser = NotationParser::new();
        let expression = parser.parse(input).unwrap();
        assert_eq!(expression.to_string(), "2 ** 6");
        assert_eq!(
            expression,
            Expression::Infix(
                Operator::Pow,
                &Expression::Value(2.0),
                &Expression::Value(6.0)
            )
        )
    }

    #[test]
    fn test_parens_expressions() {
        #[rustfmt::skip]
        let inputs = [
            ( "(1)", parens(val(1.0)).clone()),
            ( "(1 + 1)", parens(infix(Operator::Add, val(1.0), val(1.0))).clone()),
        ];

        for (input, expected) in inputs {
            let mut parser = NotationParser::new();
            let expression = parser.parse(input).unwrap();
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
            let mut parser = NotationParser::new();
            let expression = parser.parse(input).unwrap();
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
            let mut parser = NotationParser::new();
            let expression = parser.parse(input).unwrap();
            assert_eq!(expression, expected);
            assert_eq!(expression.to_string(), input);
        }
    }

    #[test]
    fn test_group_expressions() {
        #[rustfmt::skip]
        let inputs = [
            ( "{1, 5}", Expression::Group(vec![val(1.0).clone(), val(5.0).clone()], vec![])),
            ( "{1 % 3, 5}", Expression::Group(vec![infix(Operator::Rem, val(1.0), val(3.0)).clone(), val(5.0).clone()], vec![])),
            ( "{1, 5}dl1", Expression::Group(vec![val(1.0).clone(), val(5.0).clone()], vec![Modifier::Drop(crate::KeepKind::Lowest, 1)])),
            ( "{1, 5}kh3dl1", Expression::Group(vec![val(1.0).clone(), val(5.0).clone()], vec![Modifier::Keep(crate::KeepKind::Highest, 3), Modifier::Drop(crate::KeepKind::Lowest, 1)])),
        ];

        for (input, expected) in inputs {
            let mut parser = NotationParser::new();
            let expression = parser.parse(input).unwrap();
            assert_eq!(expression, expected);
            assert_eq!(expression.to_string(), input);
        }
    }
}
