mod evaluate;
mod parse;

pub use evaluate::expression::RolledExpression;
pub use parse::{ComparePoint, Dice, DiceKind, Expression, KeepKind, MathFn1, MathFn2, Modifier};

/// Parses the notation returning the parsed abstract syntax tree without
/// rolling the dice.
pub fn parse(notation: &str) -> Result<Expression, String> {
    Expression::parse(notation)
}

/// Parses the notation returning the result of rolling all the dice parsed.
pub fn roll(notation: &str) -> Result<RolledExpression, String> {
    let expression = Expression::parse(notation)?;
    Ok(expression.roll(&mut rand::thread_rng()))
}

/// Same as `roll()` but allows you to choose the rng you prefer to use.
pub fn roll_with(notation: &str, rng: &mut impl rand::Rng) -> Result<RolledExpression, String> {
    let expression = Expression::parse(notation)?;
    Ok(expression.roll(rng))
}

#[cfg(test)]
mod tests {
    use rand::{rngs::StdRng, SeedableRng};

    use super::*;

    #[test]
    fn test_running() {
        let input = "999d444k3s";

        let mut rng = StdRng::seed_from_u64(1);
        let parsed = Expression::parse(input).unwrap();
        let rolled = parsed.roll(&mut rng);
        let total = rolled.value();

        let expected_rolls = "[1d, 1d, 2d, 3d, 3d, 3d, 3d, 4d, 4d, 4d, 4d, 4d, 5d, 5d, 6d, 6d, 6d, 6d, 7d, 9d, 9d, 10d, 12d, 12d, 13d, 13d, 13d, 14d, 14d, 15d, 15d, 15d, 16d, 16d, 16d, 18d, 18d, 18d, 19d, 19d, 19d, 19d, 19d, 20d, 20d, 20d, 21d, 21d, 21d, 22d, 22d, 22d, 23d, 23d, 23d, 23d, 23d, 23d, 23d, 24d, 25d, 25d, 25d, 25d, 25d, 25d, 26d, 26d, 26d, 27d, 28d, 28d, 28d, 28d, 28d, 29d, 30d, 31d, 31d, 32d, 32d, 32d, 32d, 33d, 33d, 33d, 33d, 33d, 33d, 34d, 34d, 34d, 35d, 35d, 35d, 35d, 36d, 36d, 36d, 37d, 37d, 37d, 38d, 41d, 42d, 42d, 42d, 43d, 43d, 44d, 45d, 45d, 45d, 45d, 47d, 47d, 49d, 49d, 49d, 50d, 50d, 50d, 50d, 51d, 51d, 52d, 53d, 53d, 53d, 54d, 54d, 54d, 54d, 54d, 55d, 55d, 55d, 55d, 55d, 56d, 56d, 57d, 57d, 57d, 57d, 57d, 58d, 58d, 59d, 61d, 61d, 62d, 63d, 63d, 64d, 64d, 64d, 64d, 64d, 65d, 65d, 66d, 67d, 67d, 67d, 69d, 69d, 69d, 69d, 70d, 70d, 70d, 70d, 70d, 71d, 71d, 72d, 72d, 72d, 72d, 72d, 72d, 72d, 72d, 73d, 73d, 73d, 74d, 75d, 75d, 75d, 76d, 76d, 77d, 77d, 78d, 78d, 78d, 79d, 80d, 80d, 80d, 81d, 81d, 81d, 81d, 82d, 82d, 83d, 84d, 85d, 85d, 86d, 86d, 86d, 87d, 87d, 88d, 89d, 89d, 89d, 89d, 90d, 90d, 90d, 91d, 91d, 91d, 91d, 91d, 91d, 92d, 92d, 93d, 93d, 93d, 94d, 94d, 96d, 97d, 97d, 97d, 97d, 97d, 98d, 98d, 99d, 100d, 100d, 101d, 101d, 102d, 102d, 103d, 103d, 105d, 105d, 105d, 105d, 106d, 106d, 107d, 107d, 108d, 108d, 109d, 109d, 110d, 110d, 113d, 113d, 113d, 114d, 114d, 114d, 114d, 115d, 115d, 116d, 116d, 116d, 117d, 117d, 117d, 118d, 118d, 118d, 118d, 119d, 119d, 120d, 121d, 121d, 122d, 123d, 124d, 124d, 125d, 125d, 125d, 125d, 126d, 126d, 126d, 127d, 127d, 128d, 128d, 129d, 129d, 129d, 129d, 130d, 131d, 131d, 132d, 133d, 133d, 134d, 134d, 134d, 134d, 134d, 134d, 135d, 135d, 135d, 136d, 137d, 139d, 140d, 141d, 141d, 141d, 141d, 141d, 141d, 142d, 142d, 142d, 143d, 143d, 143d, 143d, 143d, 144d, 144d, 144d, 145d, 145d, 146d, 146d, 147d, 147d, 147d, 147d, 148d, 148d, 149d, 149d, 150d, 150d, 150d, 151d, 152d, 152d, 152d, 152d, 152d, 153d, 154d, 155d, 157d, 157d, 157d, 157d, 157d, 158d, 159d, 159d, 159d, 160d, 161d, 162d, 162d, 162d, 163d, 163d, 164d, 164d, 164d, 165d, 165d, 166d, 166d, 166d, 166d, 166d, 167d, 167d, 168d, 169d, 169d, 169d, 169d, 169d, 170d, 171d, 172d, 172d, 173d, 173d, 174d, 174d, 174d, 174d, 174d, 175d, 175d, 177d, 177d, 177d, 177d, 177d, 177d, 177d, 178d, 180d, 181d, 182d, 182d, 182d, 182d, 182d, 183d, 184d, 184d, 184d, 184d, 184d, 185d, 185d, 186d, 186d, 186d, 186d, 187d, 187d, 187d, 188d, 188d, 188d, 189d, 189d, 190d, 190d, 190d, 191d, 191d, 192d, 192d, 192d, 192d, 193d, 193d, 193d, 193d, 193d, 193d, 194d, 194d, 194d, 195d, 195d, 195d, 195d, 196d, 196d, 198d, 198d, 198d, 199d, 199d, 199d, 200d, 200d, 201d, 201d, 201d, 203d, 203d, 204d, 204d, 205d, 206d, 206d, 207d, 207d, 208d, 208d, 209d, 209d, 211d, 211d, 212d, 212d, 213d, 213d, 213d, 213d, 214d, 214d, 216d, 218d, 218d, 218d, 219d, 219d, 219d, 219d, 220d, 221d, 222d, 222d, 222d, 222d, 223d, 224d, 224d, 224d, 225d, 225d, 226d, 226d, 226d, 226d, 228d, 229d, 230d, 230d, 230d, 231d, 231d, 231d, 231d, 232d, 232d, 232d, 232d, 233d, 234d, 235d, 235d, 235d, 236d, 237d, 237d, 237d, 237d, 237d, 237d, 238d, 238d, 239d, 240d, 240d, 241d, 241d, 241d, 241d, 241d, 242d, 242d, 242d, 242d, 243d, 244d, 245d, 245d, 248d, 249d, 249d, 250d, 251d, 252d, 252d, 252d, 252d, 252d, 254d, 254d, 254d, 254d, 255d, 256d, 256d, 257d, 257d, 257d, 258d, 258d, 259d, 259d, 260d, 261d, 261d, 262d, 262d, 263d, 263d, 263d, 264d, 266d, 266d, 266d, 266d, 266d, 268d, 269d, 269d, 270d, 270d, 270d, 271d, 271d, 271d, 271d, 271d, 271d, 272d, 273d, 274d, 274d, 274d, 274d, 275d, 275d, 276d, 276d, 277d, 277d, 278d, 278d, 279d, 280d, 281d, 281d, 281d, 281d, 283d, 283d, 285d, 285d, 285d, 286d, 286d, 286d, 286d, 287d, 287d, 287d, 287d, 287d, 287d, 288d, 288d, 288d, 288d, 291d, 291d, 292d, 292d, 292d, 292d, 292d, 292d, 293d, 293d, 294d, 294d, 294d, 295d, 295d, 295d, 296d, 296d, 297d, 297d, 297d, 298d, 298d, 299d, 299d, 301d, 301d, 303d, 304d, 305d, 305d, 305d, 305d, 305d, 306d, 306d, 306d, 307d, 307d, 307d, 307d, 308d, 308d, 308d, 309d, 309d, 310d, 310d, 310d, 310d, 310d, 312d, 313d, 313d, 313d, 313d, 314d, 314d, 314d, 315d, 315d, 315d, 315d, 316d, 316d, 317d, 317d, 317d, 317d, 317d, 317d, 320d, 320d, 321d, 322d, 322d, 323d, 323d, 325d, 325d, 325d, 327d, 328d, 328d, 328d, 328d, 328d, 329d, 330d, 332d, 332d, 333d, 334d, 334d, 335d, 335d, 335d, 336d, 336d, 337d, 338d, 339d, 339d, 340d, 341d, 342d, 342d, 343d, 343d, 343d, 344d, 344d, 344d, 345d, 345d, 345d, 345d, 346d, 346d, 347d, 347d, 348d, 348d, 348d, 348d, 349d, 349d, 349d, 350d, 350d, 350d, 351d, 351d, 351d, 351d, 352d, 352d, 352d, 353d, 353d, 354d, 354d, 355d, 356d, 356d, 356d, 356d, 357d, 357d, 358d, 358d, 359d, 359d, 360d, 361d, 361d, 362d, 362d, 363d, 363d, 363d, 363d, 363d, 365d, 365d, 366d, 366d, 366d, 367d, 367d, 368d, 368d, 368d, 369d, 369d, 369d, 370d, 370d, 370d, 370d, 371d, 371d, 371d, 371d, 372d, 372d, 372d, 372d, 374d, 374d, 375d, 375d, 375d, 377d, 377d, 378d, 378d, 378d, 378d, 380d, 381d, 381d, 382d, 382d, 383d, 384d, 384d, 384d, 385d, 385d, 386d, 386d, 387d, 387d, 387d, 388d, 388d, 389d, 389d, 389d, 389d, 390d, 390d, 390d, 391d, 391d, 391d, 392d, 392d, 394d, 395d, 395d, 395d, 396d, 397d, 397d, 399d, 401d, 401d, 402d, 402d, 402d, 403d, 404d, 404d, 405d, 405d, 405d, 407d, 407d, 407d, 407d, 408d, 409d, 409d, 409d, 410d, 410d, 410d, 411d, 411d, 411d, 412d, 412d, 412d, 413d, 413d, 413d, 413d, 414d, 414d, 414d, 415d, 416d, 416d, 417d, 418d, 418d, 419d, 419d, 421d, 422d, 423d, 424d, 424d, 425d, 425d, 425d, 426d, 427d, 428d, 428d, 428d, 429d, 429d, 430d, 430d, 430d, 431d, 431d, 431d, 432d, 432d, 432d, 433d, 433d, 433d, 433d, 435d, 435d, 436d, 436d, 437d, 437d, 437d, 437d, 438d, 439d, 440d, 440d, 441d, 441d, 441d, 441d, 441d, 442d, 443, 443d, 443d, 444, 444]";
        let expected_total = 1331.0;

        assert_eq!(expected_rolls, rolled.to_string());
        assert_eq!(expected_total, total);
    }
}
