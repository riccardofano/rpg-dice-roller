use std::f64::consts::E;

use rand::Rng;

use crate::{
    parse::{Expression, MathFn1, MathFn2, Operator},
    Dice, DiceKind, Modifier,
};

use super::{
    group_rolls::apply_group_modifiers,
    roll::{GroupRollOutput, RollOutput},
};

impl Expression {
    pub fn roll(self, rng: &mut impl Rng) -> RolledExpression {
        match self {
            Expression::Value(float) => RolledExpression::Value(float),
            Expression::DiceFudge1(qty, mods) => {
                let dice = dice_from_expression(qty.map(|q| *q), DiceKind::Fudge1, mods, rng);
                RolledExpression::DiceRoll(dice.roll_all(rng))
            }
            Expression::DiceFudge2(qty, mods) => {
                let dice = dice_from_expression(qty.map(|q| *q), DiceKind::Fudge2, mods, rng);
                RolledExpression::DiceRoll(dice.roll_all(rng))
            }
            Expression::DicePercentile(qty, mods) => {
                let dice =
                    dice_from_expression(qty.map(|q| *q), DiceKind::Standard(100), mods, rng);
                RolledExpression::DiceRoll(dice.roll_all(rng))
            }
            Expression::DiceStandard(qty, sides, mods) => {
                let sides = sides.roll(rng).value().round() as u32;
                let dice =
                    dice_from_expression(qty.map(|q| *q), DiceKind::Standard(sides), mods, rng);
                RolledExpression::DiceRoll(dice.roll_all(rng))
            }
            Expression::Parens(expr) => expr.roll(rng),
            Expression::Group(expressions, modifiers) => {
                let rolled_expressions =
                    expressions.into_iter().map(|expr| expr.roll(rng)).collect();
                let output = apply_group_modifiers(rolled_expressions, &modifiers);
                RolledExpression::Group(output)
            }
            Expression::Infix(op, lhs, rhs) => {
                RolledExpression::Infix(op, Box::new(lhs.roll(rng)), Box::new(rhs.roll(rng)))
            }
            Expression::Fn1(f, arg) => RolledExpression::Fn1(f, Box::new(arg.roll(rng))),
            Expression::Fn2(f, arg1, arg2) => {
                RolledExpression::Fn2(f, Box::new(arg1.roll(rng)), Box::new(arg2.roll(rng)))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum RolledExpression {
    DiceRoll(RollOutput),
    Group(GroupRollOutput),
    Value(f64),
    Parens(Box<RolledExpression>),
    Infix(Operator, Box<RolledExpression>, Box<RolledExpression>),
    Fn1(MathFn1, Box<RolledExpression>),
    Fn2(MathFn2, Box<RolledExpression>, Box<RolledExpression>),
}

impl RolledExpression {
    pub fn value(&self) -> f64 {
        match self {
            RolledExpression::DiceRoll(roll_output) => roll_output.value(),
            RolledExpression::Group(group_output) => group_output.value(),
            RolledExpression::Value(float) => *float,
            RolledExpression::Parens(expr) => expr.value(),
            RolledExpression::Infix(operator, lhs, rhs) => operator.evaluate(lhs, rhs),
            RolledExpression::Fn1(f, arg) => f.evaluate(arg),
            RolledExpression::Fn2(f, arg1, arg2) => f.evaluate(arg1, arg2),
        }
    }
}

fn dice_from_expression(
    quantity: Option<Expression>,
    kind: DiceKind,
    modifiers: Vec<Modifier>,
    rng: &mut impl Rng,
) -> Dice {
    let quantity = match quantity {
        Some(expression) => expression.roll(rng).value().round() as u32,
        None => 1,
    };

    Dice::new(quantity, kind, modifiers)
}

impl Operator {
    pub fn evaluate(&self, lhs: &RolledExpression, rhs: &RolledExpression) -> f64 {
        match self {
            Operator::Add => lhs.value() + rhs.value(),
            Operator::Sub => lhs.value() - rhs.value(),
            Operator::Mul => lhs.value() * rhs.value(),
            Operator::Div => lhs.value() / rhs.value(),
            Operator::Pow => lhs.value() * rhs.value(),
            Operator::Rem => lhs.value() % rhs.value(),
        }
    }
}

impl MathFn1 {
    pub fn evaluate(&self, arg: &RolledExpression) -> f64 {
        let arg = arg.value();
        match self {
            MathFn1::Abs => arg.abs(),
            MathFn1::Floor => arg.floor(),
            MathFn1::Ceil => arg.ceil(),
            MathFn1::Round => arg.round(),
            MathFn1::Sign => arg.signum(),
            MathFn1::Sqrt => arg.sqrt(),
            MathFn1::Log => arg.log(E),
            MathFn1::Exp => arg.exp(),
            MathFn1::Sin => arg.sin(),
            MathFn1::Cos => arg.cos(),
            MathFn1::Tan => arg.tan(),
        }
    }
}

impl MathFn2 {
    pub fn evaluate(&self, arg1: &RolledExpression, arg2: &RolledExpression) -> f64 {
        let arg1 = arg1.value();
        let arg2 = arg2.value();

        match self {
            MathFn2::Min => arg1.min(arg2),
            MathFn2::Max => arg1.max(arg2),
            MathFn2::Pow => arg1.powf(arg2),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Value(val) => write!(f, "{val}"),
            Expression::DiceStandard(None, sides, _) => write!(f, "d{sides}"),
            Expression::DiceStandard(Some(qty), sides, _) => write!(f, "{qty}d{sides}"),
            Expression::DiceFudge1(None, _) => write!(f, "dF.1"),
            Expression::DiceFudge1(Some(qty), _) => write!(f, "{qty}dF.1"),
            Expression::DiceFudge2(None, _) => write!(f, "dF.2"),
            Expression::DiceFudge2(Some(qty), _) => write!(f, "{qty}dF.2"),
            Expression::DicePercentile(None, _) => write!(f, "d%"),
            Expression::DicePercentile(Some(qty), _) => write!(f, "{qty}d%"),
            Expression::Parens(expr) => write!(f, "({expr})"),
            Expression::Group(expressions, _) => {
                let expressions = expressions
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{expressions}}}",)
            }
            Expression::Infix(op, expr1, expr2) => write!(f, "{expr1} {op} {expr2}"),
            // no parens on the function call because there's always a parens expression following the function call
            Expression::Fn1(func, arg) => write!(f, "{func}{arg}"),
            Expression::Fn2(func, arg1, arg2) => write!(f, "{func}({arg1}, {arg2})"),
        }
    }
}

impl std::fmt::Display for RolledExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RolledExpression::DiceRoll(roll_output) => write!(f, "{roll_output}"),
            RolledExpression::Group(group_output) => write!(f, "{{{group_output}}}"),
            RolledExpression::Value(float) => write!(f, "{float}"),
            RolledExpression::Parens(expr) => write!(f, "({expr})"),
            RolledExpression::Infix(op, lhs, rhs) => write!(f, "{lhs} {op} {rhs}"),
            RolledExpression::Fn1(func, arg) => write!(f, "{func}{arg}"),
            RolledExpression::Fn2(func, arg1, arg2) => write!(f, "{func}({arg1}, {arg2})"),
        }
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Rem => "%",
            Operator::Pow => "**",
        };
        write!(f, "{str}")
    }
}

impl std::fmt::Display for MathFn1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            MathFn1::Abs => "abs",
            MathFn1::Floor => "floor",
            MathFn1::Ceil => "ceil",
            MathFn1::Round => "round",
            MathFn1::Sign => "sign",
            MathFn1::Sqrt => "sqrt",
            MathFn1::Log => "ln",
            MathFn1::Exp => "exp",
            MathFn1::Sin => "sin",
            MathFn1::Cos => "cos",
            MathFn1::Tan => "tan",
        };
        write!(f, "{str}")
    }
}

impl std::fmt::Display for MathFn2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            MathFn2::Min => "min",
            MathFn2::Max => "max",
            MathFn2::Pow => "pow",
        };
        write!(f, "{str}")
    }
}
