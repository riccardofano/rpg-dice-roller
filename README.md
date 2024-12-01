# rpg-dice-roller

`rpg-dice-roller` allows you to simulate dice rolls with modifiers and mathematical expressions.\
This crate is inspired by and based on the popular JavaScript library [rpg-dice-roller](https://github.com/dice-roller/rpg-dice-roller), please check out their [wonderful documentation](https://dice-roller.github.io/documentation/guide/) for more details on the functionality provided by the library.

## Installation
```
cargo add rpg-dice-roller
```

## Example Usage
```rust
use rpg-dice-roller::roll;

// Roll 4 d10.
let rolled = roll("4d10")?;
println!("{rolled} = {}", rolled.value()); // [3, 2, 3, 3] = 11

// Roll 3 d20, make the minimum value 5, keep the lowest 2 rolls.
let rolled = roll("3d20min5kl2")?;
println!("{rolled} = {}", rolled.value()); // [6, 5^, 17d] = 11

// Roll a group of 3 expressions:
// 3d100,
// 3 fudge dice (d6s with 2 -1 sides, 2 +1 sides and 2 blank ones) and add 5 to the result,
// 1 d50 and evalate its result to the power of 2
// Count the number of expressions whose value was greater than 50
let rolled = roll("{3d%, 3dF.2 + 5, pow(d50, 2)}>50")?;
println!("{rolled} = {}", rolled.value()); // {[68, 8, 31]*, [1, 1, 1] + 5, pow([14], 2)*} = 2
```

## Supported Die kinds
More details on the [dice kind documentation](https://docs.rs/rpg-dice-roller/latest/rpg_dice_roller/enum.DiceKind.html)
- Any dice until u32::MAX number of sides
- Fudge/Fate die with 4 blank sides, 1 plus side and 1 minus side.
- Fudge/Fate die with 2 blank sides, 2 plus sides and 2 minus sides.

## Supported Modifiers
More details in the [modifier documentation](https://docs.rs/rpg-dice-roller/latest/rpg_dice_roller/enum.Modifier.html)
- Min/Max
- Exploding/Penetrating/Compounding/PenetratingCompounding
- Reroll/Reroll once
- Unique/Unique once
- TargetSuccess/Failure
- CriticalSuccess/Failure
- Keep/Drop
- Sort

## Supported expressions
- Addition `+`, subtraction `-`, multiplication `*`, division `/`, remainder `%`, power `^` or `**`.
- `abs()`, `floor()`, `ceil()`, `round()`, `sign()`, `sqrt()`, `log(E)`, `exp()`, `sin()`, `cos()`, `tan()`.
- `min(_, _)`, `max(_, _)`, `pow(_, _)`.
- operation precedence between operators and parethesis.
- Groups `{}` will sum (or apply target success/failure if modifiers were applied) to the comma separated expressions inside.

## Rng
This crates uses `rand::thread_rng()` by default but provides `_with` functions (`roll_with`, `Dice::roll_once_with()`, etc.) so you can use anything the `rand::Rng` trait with them.
