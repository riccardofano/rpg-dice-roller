use crate::{KeepKind, Modifier};

use super::dice_roll::RollOutput;

pub fn apply_group_modifiers(rolls: &mut [RollOutput], modifiers: &[Modifier]) {
    for modifier in modifiers {
        match modifier {
            Modifier::TargetSuccess(compare_point) => todo!(),
            Modifier::TargetFailure(compare_point, compare_point1) => todo!(),
            Modifier::Keep(keep_kind, amount) => todo!(),
            Modifier::Drop(keep_kind, amount) => todo!(),
            Modifier::Sort(sort_kind) => todo!(),
            m => unreachable!("{m:?} modifier is not allowed for groups"),
        }
    }
}
