use crate::parse::{ComparePoint, KeepKind, Modifier, SortKind};

use super::{
    expression::RolledExpression,
    roll::{GroupRollOutput, ModifierFlags, RollOutputKind},
};

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

// TODO: store the total value of each expression so I don't have to recalculate it constantly
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
        GroupRollOutput::set_modifier_flag(
            &mut output.modifier_flags[i],
            ModifierFlags::Drop as u8,
        );
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
        GroupRollOutput::set_modifier_flag(
            &mut output.modifier_flags[i],
            ModifierFlags::Drop as u8,
        );
    }
}

fn apply_group_target_success(output: &mut GroupRollOutput, compare_point: ComparePoint) {
    let cmp_fn = compare_point.compare_fn();

    for (i, expression) in output.expressions.iter().enumerate() {
        if cmp_fn(expression.value()) {
            GroupRollOutput::set_modifier_flag(
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
            GroupRollOutput::set_modifier_flag(
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
