use std::f64::consts::E;

use crate::parse::{Expression, MathFn1, MathFn2, Operator};

use super::{dice_roll::to_notations, group_rolls::to_group_notations};

impl Expression {
    pub fn evaluate(self) -> f64 {
        match self {
            Expression::Value(float) => float,
            Expression::DiceRolls(output) => output.value(),
            Expression::Parens(expr) => expr.evaluate(),
            Expression::Group(outputs) => outputs.into_iter().map(|output| output.value()).sum(),
            Expression::Infix(op, lhs, rhs) => op.evaluate(*lhs, *rhs),
            Expression::Fn1(f, arg) => f.evaluate(*arg),
            Expression::Fn2(f, arg1, arg2) => f.evaluate(*arg1, *arg2),
        }
    }
}

impl Operator {
    pub fn evaluate(self, lhs: Expression, rhs: Expression) -> f64 {
        match self {
            Operator::Add => lhs.evaluate() + rhs.evaluate(),
            Operator::Sub => lhs.evaluate() - rhs.evaluate(),
            Operator::Mul => lhs.evaluate() * rhs.evaluate(),
            Operator::Div => lhs.evaluate() / rhs.evaluate(),
            Operator::Pow => lhs.evaluate() * rhs.evaluate(),
            Operator::Rem => lhs.evaluate() % rhs.evaluate(),
        }
    }
}

impl MathFn1 {
    pub fn evaluate(self, arg: Expression) -> f64 {
        let arg = arg.evaluate();
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
    pub fn evaluate(self, arg1: Expression, arg2: Expression) -> f64 {
        let arg1 = arg1.evaluate();
        let arg2 = arg2.evaluate();

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
            Expression::DiceRolls(output) => write!(f, "{}", to_notations(&output.rolls)),
            Expression::Parens(expr) => write!(f, "({expr})"),
            Expression::Group(outputs) => write!(f, "{{{}}}", to_group_notations(&outputs)),
            Expression::Infix(op, expr1, expr2) => write!(f, "{expr1} {op} {expr2}"),
            // no parens on the function call because there's always a parens expression following the function call
            Expression::Fn1(func, arg) => write!(f, "{func}{arg}"),
            Expression::Fn2(func, arg1, arg2) => write!(f, "{func}{arg1}, {arg2}"),
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
