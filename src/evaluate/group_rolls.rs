use crate::{KeepKind, Modifier};

use super::roll::{ModifierFlags, RollOutput};

pub fn apply_group_modifiers(rolls: &mut [RollOutput], modifiers: &[Modifier]) {
    for modifier in modifiers {
        match modifier {
            Modifier::TargetSuccess(compare_point) => todo!(),
            Modifier::TargetFailure(compare_point, compare_point1) => todo!(),
            Modifier::Keep(keep_kind, amount) => apply_group_keep(rolls, *keep_kind, *amount),
            Modifier::Drop(keep_kind, amount) => todo!(),
            Modifier::Sort(sort_kind) => todo!(),
            m => unreachable!("{m:?} modifier is not allowed for groups"),
        }
    }
}

fn apply_group_keep(rolls: &mut [RollOutput], keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..rolls.len()).collect();
    match keep_kind {
        KeepKind::Highest => {
            indices.sort_by(|&ia, &ib| rolls[ib].value().total_cmp(&rolls[ia].value()))
        }
        KeepKind::Lowest => {
            indices.sort_by(|&ia, &ib| rolls[ia].value().total_cmp(&rolls[ib].value()))
        }
    }

    for &i in &indices[(amount as usize)..] {
        rolls[i].set_modifier_flag(ModifierFlags::Drop as u8);
    }
}

fn apply_group_drop(rolls: &mut [RollOutput], keep_kind: KeepKind, amount: u32) {
    let mut indices: Vec<usize> = (0..rolls.len()).collect();
    match keep_kind {
        KeepKind::Highest => {
            indices.sort_by(|&ia, &ib| rolls[ib].value().total_cmp(&rolls[ia].value()))
        }
        KeepKind::Lowest => {
            indices.sort_by(|&ia, &ib| rolls[ia].value().total_cmp(&rolls[ib].value()))
        }
    }

    for &i in &indices[..(amount as usize)] {
        rolls[i].set_modifier_flag(ModifierFlags::Drop as u8);
    }
}
