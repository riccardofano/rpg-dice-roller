use crate::evaluate::dice_roll::to_notations;

use super::expression::RolledExpression;

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
pub enum ModifierFlags {
    Min,
    Max,
    ExplodingStandard,
    ExplodingPenetrating,
    ExplodingCompounding,
    ExplodingPenetratingCompounding,
    ReRoll,
    ReRollOnce,
    Unique,
    UniqueOnce,
    Drop,
    TargetSuccess,
    TargetFailure,
    CriticalSuccess,
    CriticalFailure,
}

pub const MODIFER_NOTATION: [&str; 16] = [
    "^", "v", "!", "!p", "!!", "!!p", "r", "ro", "u", "uo", "d", "d", "*", "_", "**", "__",
];

#[derive(Debug, Clone, Copy)]
pub struct Roll {
    pub(crate) value: i32,
    pub(crate) modifier_flags: u32,
}

impl Roll {
    pub fn new(value: i32) -> Self {
        Self {
            value,
            modifier_flags: 0,
        }
    }

    pub fn set_modifier_flag(&mut self, modifier_flag: u8) {
        self.modifier_flags |= 1 << modifier_flag;
    }

    pub fn was_modifier_applied(&self, modifier_flag: u8) -> bool {
        (self.modifier_flags & (1 << modifier_flag)) != 0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RollOutput {
    pub(crate) rolls: Vec<Roll>,
    pub(crate) kind: RollOutputKind,
    pub(crate) modifier_flags: u32,
}

impl RollOutput {
    pub fn new(rolls: Vec<Roll>, kind: RollOutputKind) -> Self {
        Self {
            rolls,
            kind,
            modifier_flags: 0,
        }
    }

    #[rustfmt::skip]
    pub fn value(&self) -> f64 {
        let iter = self
            .rolls
            .iter()
            .filter(|r| !r.was_modifier_applied(ModifierFlags::Drop as u8));

        match self.kind {
            RollOutputKind::Sum => iter.map(|r| r.value as f64).sum(),
            RollOutputKind::TargetSuccess => iter
                .map(|r| {
                    let success = r.was_modifier_applied(ModifierFlags::TargetSuccess as u8);
                    if success { 1.0 } else { 0.0 }
                })
                .sum(),
            RollOutputKind::TargetFailure => iter
                .map(|r| {
                    let success = r.was_modifier_applied(ModifierFlags::TargetSuccess as u8);
                    let failure = r.was_modifier_applied(ModifierFlags::TargetFailure as u8);
                    if success { 1.0 } else if failure { -1.0 } else { 0.0 }
                })
                .sum(),
        }
    }

    pub fn set_modifier_flag(&mut self, modifier_flag: u8) {
        self.modifier_flags |= 1 << modifier_flag;
    }

    pub fn was_modifier_applied(&self, modifier_flag: u8) -> bool {
        (self.modifier_flags & (1 << modifier_flag)) != 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RollOutputKind {
    Sum,
    TargetSuccess,
    TargetFailure,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupRollOutput {
    pub(crate) modifier_flags: Vec<u32>,
    pub(crate) expressions: Vec<RolledExpression>,
    pub(crate) kind: RollOutputKind,
}

impl GroupRollOutput {
    #[rustfmt::skip]
    pub fn value(&self) -> f64 {
        let iter = self.expressions.iter().enumerate().filter(|(i, _)| {
            !Self::was_modifier_applied(self.modifier_flags[*i], ModifierFlags::Drop as u8)
        });

        match self.kind {
            RollOutputKind::Sum => iter.map(|(_, expr)| expr.value()).sum(),
            RollOutputKind::TargetSuccess => iter
                .map(|(i, _)| {
                    let success = Self::was_modifier_applied(self.modifier_flags[i], ModifierFlags::TargetSuccess as u8);
                    if success { 1.0 } else { 0.0 }
                })
                .sum(),
            RollOutputKind::TargetFailure => iter
                .map(|(i, _)| {
                    let success = Self::was_modifier_applied(self.modifier_flags[i], ModifierFlags::TargetSuccess as u8);
                    let failure = Self::was_modifier_applied(self.modifier_flags[i], ModifierFlags::TargetFailure as u8);
                    if success { 1.0 } else if failure { -1.0 } else { 0.0 }
                })
                .sum(),
        }
    }

    // NOTE: This is not a method of GroupRollOutput because otherwise I would not
    // be allowed to mutably borrow self while iterating over self.expressions
    pub fn set_modifier_flag(flags: &mut u32, flag: u8) {
        *flags |= 1 << flag;
    }
    pub fn was_modifier_applied(flags: u32, flag: u8) -> bool {
        (flags & (1 << flag)) != 0
    }
}

impl PartialEq for Roll {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl Eq for Roll {}
impl Ord for Roll {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}
impl PartialOrd for Roll {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::fmt::Display for Roll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = format!("{}", self.value);
        for (i, notation) in MODIFER_NOTATION.iter().enumerate() {
            if self.was_modifier_applied(i as u8) {
                str.push_str(notation);

                if ModifierFlags::Drop as u8 == i as u8 {
                    // We don't want to apply critical success/failure on dropped rolls
                    break;
                }
            }
        }
        write!(f, "{}", str)
    }
}

impl std::fmt::Display for RollOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = to_notations(&self.rolls);

        for (i, notation) in MODIFER_NOTATION.iter().enumerate() {
            if self.was_modifier_applied(i as u8) {
                str.push_str(notation);
            }
        }
        write!(f, "{}", str)
    }
}

impl std::fmt::Display for GroupRollOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let group = self
            .expressions
            .iter()
            .enumerate()
            .map(|(ei, expr)| {
                let mut notations = String::new();
                for (ni, notation) in MODIFER_NOTATION.iter().enumerate() {
                    if GroupRollOutput::was_modifier_applied(self.modifier_flags[ei], ni as u8) {
                        notations.push_str(notation);
                    }
                }
                format!("{expr}{notations}")
            })
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "{group}")
    }
}
