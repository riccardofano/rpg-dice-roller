mod evaluate;
mod parse;

pub use parse::*;

#[cfg(test)]
mod tests {
    use std::time::Instant;

    use rand::thread_rng;

    use super::*;

    #[test]
    fn test_running() {
        let input = "999d444k3s";
        let instant = Instant::now();
        let expression = Expression::parse(input).unwrap();
        let str_expression = expression.to_string();
        // let evaluated = expression.roll(&mut thread_rng());
        let elapsed = instant.elapsed();

        // println!("{elapsed:#?} - {input}: {str_expression} = {evaluated}");
    }
}
