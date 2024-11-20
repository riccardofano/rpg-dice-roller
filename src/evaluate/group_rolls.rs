use crate::{parse::SortKind, ComparePoint, KeepKind, Modifier};

use super::{
    expression::RolledExpression,
    roll::{ModifierFlags, RollOutputKind},
};

#[derive(Debug, Clone)]
pub struct GroupRollOutput {
    modifier_flags: Vec<u32>,
    expressions: Vec<RolledExpression>,
    kind: RollOutputKind,
}

impl GroupRollOutput {
    pub fn value(&self) -> f64 {
        match self.kind {
            RollOutputKind::Sum => todo!(),
            RollOutputKind::TargetSuccess => todo!(),
            RollOutputKind::TargetFailure => todo!(),
        }
    }
}

// NOTE: This is not a method of GroupRollOutput because otherwise I would not
// be allowed to mutably borrow self while iterating over self.expressions
pub fn set_modifier_flag(flags: &mut u32, flag: u8) {
    *flags |= 1 << flag;
}

pub fn apply_group_modifiers(
    expressions: Vec<RolledExpression>,
    modifiers: &[Modifier],
) -> GroupRollOutput {
    let mut output = GroupRollOutput {
        modifier_flags: vec![0; expressions.len()],
        expressions,
        kind: RollOutputKind::Sum,
    };

    for modifier in modifiers {
        match modifier {
            Modifier::TargetSuccess(compare_point) => {
                apply_group_target_success(&mut output, *compare_point)
            }
            Modifier::TargetFailure(success_cmp, failure_cmp) => {
                apply_group_target_failure(&mut output, *success_cmp, *failure_cmp)
            }
            Modifier::Keep(keep_kind, amount) => apply_group_keep(&mut output, *keep_kind, *amount),
            Modifier::Drop(keep_kind, amount) => apply_group_drop(&mut output, *keep_kind, *amount),
            Modifier::Sort(sort_kind) => apply_group_sort(&mut output, *sort_kind),
            m => unreachable!("{m:?} modifier is not allowed for groups"),
        }
    }
    output
}

fn apply_group_keep(output: &mut GroupRollOutput, keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..output.expressions.len()).collect();

    match keep_kind {
        KeepKind::Highest => indices.sort_by(|&ia, &ib| {
            output.expressions[ib]
                .value()
                .total_cmp(&output.expressions[ia].value())
        }),
        KeepKind::Lowest => indices.sort_by(|&ia, &ib| {
            output.expressions[ia]
                .value()
                .total_cmp(&output.expressions[ib].value())
        }),
    }

    for &i in &indices[(amount as usize)..] {
        set_modifier_flag(&mut output.modifier_flags[i], ModifierFlags::Drop as u8);
    }
}

fn apply_group_drop(output: &mut GroupRollOutput, keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..output.expressions.len()).collect();
    match keep_kind {
        KeepKind::Highest => indices.sort_by(|&ia, &ib| {
            output.expressions[ib]
                .value()
                .total_cmp(&output.expressions[ia].value())
        }),
        KeepKind::Lowest => indices.sort_by(|&ia, &ib| {
            output.expressions[ia]
                .value()
                .total_cmp(&output.expressions[ib].value())
        }),
    }

    for &i in &indices[..(amount as usize)] {
        set_modifier_flag(&mut output.modifier_flags[i], ModifierFlags::Drop as u8);
    }
}

fn apply_group_target_success(output: &mut GroupRollOutput, compare_point: ComparePoint) {
    let cmp_fn = compare_point.compare_fn();

    for (i, expression) in output.expressions.iter().enumerate() {
        if cmp_fn(expression.value()) {
            set_modifier_flag(
                &mut output.modifier_flags[i],
                ModifierFlags::TargetSuccess as u8,
            );
        }
    }
}

fn apply_group_target_failure(
    output: &mut GroupRollOutput,
    success_cmp: ComparePoint,
    failure_cmp: ComparePoint,
) {
    apply_group_target_success(output, success_cmp);

    let cmp_fn = failure_cmp.compare_fn();
    for (i, expression) in output.expressions.iter().enumerate() {
        if cmp_fn(expression.value()) {
            set_modifier_flag(
                &mut output.modifier_flags[i],
                ModifierFlags::TargetFailure as u8,
            );
        }
    }
}

fn apply_group_sort(output: &mut GroupRollOutput, sort_kind: SortKind) {
    match sort_kind {
        SortKind::Ascending => output
            .expressions
            .sort_by(|a, b| a.value().total_cmp(&b.value())),
        SortKind::Descending => output
            .expressions
            .sort_by(|a, b| b.value().total_cmp(&a.value())),
    }

    match sort_kind {
        SortKind::Ascending => {
            for expression in output.expressions.iter_mut() {
                if let RolledExpression::DiceRoll(roll_output) = expression {
                    roll_output.rolls.sort();
                }
            }
        }
        SortKind::Descending => {
            for expression in output.expressions.iter_mut() {
                if let RolledExpression::DiceRoll(roll_output) = expression {
                    roll_output.rolls.sort_by(|a, b| b.cmp(a));
                }
            }
        }
    }
}

pub fn to_group_notations(expressions: &[RolledExpression]) -> String {
    expressions
        .iter()
        .map(|o| o.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}
