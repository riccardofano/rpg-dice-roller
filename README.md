rpg-dice-roller
==

`rpg-dice-roller` allows you to simulate dice rolls with modifiers and mathematical expressions.\
This crate is inspired by and based on the popular JavaScript library [rpg-dice-roller](https://github.com/dice-roller/rpg-dice-roller), please check out their [wonderful documentation](https://dice-roller.github.io/documentation/guide/) for more details on the functionality provided by the library.

## Installation
```
cargo add rpg-dice-roller // TODO
```

## Supported modifiers
Modifiers are special notations that appear after a dice or a dice group, and add special properties to them.

### Dice modifiers
Dice modifiers will be applied in the order of the list below even if they were written in a different order in the input string.\
If more than one of the same modifier appears in the input string, the last modifier is the one that will be applied.

- **Min** `min{amount}`\
  update the roll value to `amount` if it was **below** that value.
- **Max** `max{amount}`\
  update the roll value to `amount` if it was **above** that value.
- **Exploding** (one of:)\
  The dice explodes whenever the value passed the compare point or it's the highest value on the dice.\
  The dice can explode more than once.
  - **Standard exploding** `!` or `!{compare_point}`\
    rolls an additional dice
  - **Penetrating** `!p` or `!p{compare_point}`\
    rolls an additional dice but reduces its value by 1
  - **Compounding** `!!` or `!!{compare_point}`\
    rolls an additional dice and adds its value to the previous roll
  - **Penetrating Compounding** `!!p` or `!!p{compare_point}`\
    rolls an additional dice, reduces its value by 1 and adds its value to the previous roll
- **ReRoll** `r` or `r{compare_point}` / **Reroll once** `ro` or `ro{compare_point}`\
  rerolls the dice if it was the lowest number on the dice or it hit the compare point
- **Unique** `u` or `u{compare_point}` / **Unique once** `uo` or `uo{compare_point}`\
  rerolls the dice if the value was previously seen before
- **TargetSuccess** `{compare_point}`\
  the final roll value sum will be now determined by the amount of rolls that passed the compare point, +1 for every roll.
- **TargetFailure** `{success_compare_point}f{failure_compare_point}`\
  the final roll value sum will be now determined by the amount of rolls that passed or failed the compare points, +1 for every success roll, -1 for every fail roll.
- **CriticalSuccess** `cs` or `cs{compare_point}`\
  Purely cosmetic, adds the `**` notation if the roll was the highest value on the dice or passed the compare point.
- **CriticalFailure** `cf` or `cf{compare_point}`\
  Purely cosmetic, adds the `__ notation if the roll was the lowest value on the dice or hit the compare point.
- **Keep** `k{amount}`, `kh{amount}` or `kl{amount}`\
  (defaults to keep highest)\
  Drops every roll except the highest or lowest `{amount}`.
- **Drop** `d{amount}`, `dh{amount}` or `dl{amount}`\
  (defaults to drop lowest)\
  Drops `{amount}` of lowest or highest rolls.
- **Sort** `s`, `sa` or `sd`\
  (defaults to sort ascending)
  Sorts the roll values in ascending or descending order.

### Group modifiers
Groups are comma separated expressions surrounded by {}.\
The modifiers are used in the same way as with dice modifiers but there are fewer of them.
- Keep/Drop
- Target Success/Failure
- Sort

## Compare points
Compare point all start with a comparison sign `=`, `<>` (not equal), `<`, `>`, `<=`, `>=` and are followed by a number or an expression.
