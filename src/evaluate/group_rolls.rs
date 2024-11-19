use crate::{parse::SortKind, ComparePoint, KeepKind, Modifier};

use super::roll::{ModifierFlags, RollOutput};

pub fn apply_group_modifiers(outputs: &mut [RollOutput], modifiers: &[Modifier]) {
    for modifier in modifiers {
        match modifier {
            Modifier::TargetSuccess(compare_point) => {
                apply_group_target_success(outputs, *compare_point)
            }
            Modifier::TargetFailure(success_cmp, failure_cmp) => {
                apply_group_target_failure(outputs, *success_cmp, *failure_cmp)
            }
            Modifier::Keep(keep_kind, amount) => apply_group_keep(outputs, *keep_kind, *amount),
            Modifier::Drop(keep_kind, amount) => apply_group_drop(outputs, *keep_kind, *amount),
            Modifier::Sort(sort_kind) => apply_group_sort(outputs, *sort_kind),
            m => unreachable!("{m:?} modifier is not allowed for groups"),
        }
    }
}

fn apply_group_keep(outputs: &mut [RollOutput], keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..outputs.len()).collect();
    match keep_kind {
        KeepKind::Highest => {
            indices.sort_by(|&ia, &ib| outputs[ib].value().total_cmp(&outputs[ia].value()))
        }
        KeepKind::Lowest => {
            indices.sort_by(|&ia, &ib| outputs[ia].value().total_cmp(&outputs[ib].value()))
        }
    }

    for &i in &indices[(amount as usize)..] {
        outputs[i].set_modifier_flag(ModifierFlags::Drop as u8);
    }
}

fn apply_group_drop(outputs: &mut [RollOutput], keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..outputs.len()).collect();
    match keep_kind {
        KeepKind::Highest => {
            indices.sort_by(|&ia, &ib| outputs[ib].value().total_cmp(&outputs[ia].value()))
        }
        KeepKind::Lowest => {
            indices.sort_by(|&ia, &ib| outputs[ia].value().total_cmp(&outputs[ib].value()))
        }
    }

    for &i in &indices[..(amount as usize)] {
        outputs[i].set_modifier_flag(ModifierFlags::Drop as u8);
    }
}

fn apply_group_target_success(outputs: &mut [RollOutput], compare_point: ComparePoint) {
    let cmp_fn = compare_point.compare_fn();

    for output in outputs {
        if cmp_fn(output.value()) {
            output.set_modifier_flag(ModifierFlags::TargetSuccess as u8);
        }
    }
}

fn apply_group_target_failure(
    outputs: &mut [RollOutput],
    success_cmp: ComparePoint,
    failure_cmp: ComparePoint,
) {
    apply_group_target_success(outputs, success_cmp);

    let cmp_fn = failure_cmp.compare_fn();
    for output in outputs {
        if cmp_fn(output.value()) {
            output.set_modifier_flag(ModifierFlags::TargetFailure as u8);
        }
    }
}

fn apply_group_sort(outputs: &mut [RollOutput], sort_kind: SortKind) {
    match sort_kind {
        SortKind::Ascending => outputs.sort_by(|a, b| a.value().total_cmp(&b.value())),
        SortKind::Descending => outputs.sort_by(|a, b| b.value().total_cmp(&a.value())),
    }

    for output in outputs {
        match sort_kind {
            SortKind::Ascending => output.rolls.sort(),
            SortKind::Descending => output.rolls.sort_by(|a, b| b.cmp(a)),
        }
    }
}
