use std::ops::Not;

use rand::{rngs::StdRng, SeedableRng};
use rpg_dice_roller::{parse, roll_with};

fn test_rng() -> StdRng {
    StdRng::seed_from_u64(1)
}

fn run(input: &str, expected_rolls: &str, expected_value: f64) {
    let rolled = roll_with(input, &mut test_rng()).unwrap();
    assert_eq!(rolled.to_string(), expected_rolls);
    assert_eq!(rolled.value(), expected_value);
}

#[test]
fn test_one_d6() {
    run("d6", "[5]", 5.0);
}

#[test]
fn test_five_d6() {
    run("5d6", "[5, 2, 3, 2, 2]", 14.0);
}

#[test]
fn test_min_35() {
    run(
        "10d%min35",
        "[83, 98, 80, 70, 35^, 35^, 35^, 78, 35^, 97]",
        646.0,
    );
}

#[test]
fn test_min_last_applied() {
    run(
        "10d%min50min35",
        "[83, 98, 80, 70, 35^, 35^, 35^, 78, 35^, 97]",
        646.0,
    );
    run(
        "10d%min35min50",
        "[83, 98, 80, 70, 50^, 50^, 50^, 78, 50^, 97]",
        706.0,
    );
}

#[test]
fn test_max() {
    run(
        "10d%max80",
        "[80v, 80v, 80, 70, 20, 22, 18, 78, 26, 80v]",
        554.0,
    );
}

#[test]
fn test_max_last_applied() {
    run(
        "10d%max10max80",
        "[80v, 80v, 80, 70, 20, 22, 18, 78, 26, 80v]",
        554.0,
    );
}

#[test]
fn test_min_and_max() {
    run(
        "10d%min35max80",
        "[80v, 80v, 80, 70, 35^, 35^, 35^, 78, 35^, 80v]",
        608.0,
    );
}

#[test]
fn test_exploding_standard() {
    run("d4!", "[4!, 4!, 4!, 2]", 14.0);
}
#[test]
fn test_exploding_standard_cmp() {
    run(
        "d%!>10",
        "[83!, 98!, 80!, 70!, 20!, 22!, 18!, 78!, 26!, 97!, 80!, 100!, 12!, 53!, 16!, 39!, 55!, 21!, 42!, 7]",
        1017.0
    );
}

#[test]
fn test_exploding_penetrating() {
    run(
        "10d4!p",
        "[4!p, 3, 4!p, 1, 3, 4!p, 0, 3, 3, 1, 4!p, 3, 2, 2]",
        37.0,
    );
}

#[test]
fn test_exploding_penetrating_cmp() {
    run(
        "10d%!p<50",
        "[83, 98, 80, 70, 20!p, 21!p, 17!p, 77, 26!p, 96, 80, 100, 12!p, 52, 16!p, 38!p, 54]",
        940.0,
    );
}

#[test]
fn test_exploding_compounding() {
    run("10d4!!", "[14!!, 3, 5!!, 3, 3, 1, 10!!, 2, 2, 1]", 44.0);
}

#[test]
fn test_exploding_compounding_cmp() {
    run(
        "10d%!!<=49",
        "[83, 98, 80, 70, 138!!, 123!!, 80, 100, 65!!, 110!!]",
        947.0,
    );
}

#[test]
fn test_exploding_penetrating_compounding() {
    run(
        "10d4!!p",
        "[7!!p, 5!!p, 3, 4!!p, 3, 3, 1, 7!!p, 2, 2]",
        37.0,
    );
}

#[test]
fn test_exploding_penetrating_compounding_cmp() {
    run(
        "10d%!!p=80",
        "[83, 98, 149!!p, 20, 22, 18, 78, 26, 97, 179!!p]",
        770.0,
    );
}

#[test]
fn test_reroll() {
    run("10d4r", "[4, 4, 4, 2, 3, 4, 3r, 3, 4r, 4]", 35.0);
}

#[test]
fn test_reroll_cmp() {
    run("d%r<>80", "[80r]", 80.0);
}

#[test]
fn test_reroll_impossible_cmp() {
    run("d%r>101", "[83]", 83.0);
}

#[test]
fn test_unique() {
    run("10d4u", "[4, 2u, 3, 1u, 3u, 2u, 3u, 2u, 3u, 4u]", 27.0);
}

#[test]
fn test_unique_once() {
    run(
        "10d4uo",
        "[4, 4uo, 2, 3, 1uo, 3uo, 4uo, 2uo, 2uo, 3uo]",
        28.0,
    );
}

#[test]
fn test_unique_cmp() {
    run("10d4u>2", "[4, 2u, 3, 1u, 1u, 2u, 2, 2, 1, 2u]", 20.0);
}

#[test]
fn test_unique_once_cmp() {
    run("10d4uo<3", "[4, 4, 4, 2, 3, 4, 1, 3, 3, 4uo]", 32.0);
}

#[test]
fn test_target_success() {
    run(
        "14d4>3",
        "[4*, 4*, 4*, 2, 3, 4*, 1, 3, 3, 1, 4*, 4*, 2, 2]",
        6.0,
    );
}

#[test]
fn test_target_failure() {
    run(
        "14d4>3f=1",
        "[4*, 4*, 4*, 2, 3, 4*, 1_, 3, 3, 1_, 4*, 4*, 2, 2]",
        4.0,
    );
}

#[test]
fn test_critical_success_cmp() {
    run(
        "14d4cs>2",
        "[4**, 4**, 4**, 2, 3**, 4**, 1, 3**, 3**, 1, 4**, 4**, 2, 2]",
        41.0,
    );
}

#[test]
fn test_critical_success() {
    run(
        "14d4cs",
        "[4**, 4**, 4**, 2, 3, 4**, 1, 3, 3, 1, 4**, 4**, 2, 2]",
        41.0,
    );
}

#[test]
fn test_critical_failure() {
    run(
        "14d4cf",
        "[4, 4, 4, 2, 3, 4, 1__, 3, 3, 1__, 4, 4, 2, 2]",
        41.0,
    );
}

#[test]
fn test_critical_failure_cmp() {
    run(
        "14d4cf<3",
        "[4, 4, 4, 2__, 3, 4, 1__, 3, 3, 1__, 4, 4, 2__, 2__]",
        41.0,
    );
}

#[test]
fn test_keep() {
    run("4d50k1", "[42d, 49, 35d, 22d]", 49.0);
}

#[test]
fn test_keep_highest() {
    run("4d50kh1", "[42d, 49, 35d, 22d]", 49.0);
}

#[test]
fn test_keep_lowest() {
    run("4d50kl1", "[42d, 49d, 35d, 22]", 22.0);
}

#[test]
fn test_keep_highest_three() {
    run("4d50k3", "[42, 49, 35, 22d]", 126.0);
}

#[test]
fn test_keep_lowest_two() {
    run("4d50kl2", "[42d, 49d, 35, 22]", 57.0);
}

#[test]
fn test_drop() {
    run("4d50d1", "[42, 49, 35, 22d]", 126.0);
}

#[test]
fn test_drop_highest() {
    run("4d50dh1", "[42, 49d, 35, 22]", 99.0);
}

#[test]
fn test_drop_lowest() {
    run("4d50dl1", "[42, 49, 35, 22d]", 126.0);
}

#[test]
fn test_drop_highest_three() {
    run("4d50dh3", "[42d, 49d, 35d, 22]", 22.0);
}

#[test]
fn test_drop_lowest_two() {
    run("4d50dl2", "[42, 49, 35d, 22d]", 91.0);
}

#[test]
fn test_keep_drop() {
    run("3d10k1d1", "[9, 5d, 3d]", 9.0);
}

#[test]
fn test_sort() {
    run("3d10k1d1s", "[3d, 5d, 9]", 9.0);
}

#[test]
fn test_sort_ascending() {
    run("3d10k1d1sa", "[3d, 5d, 9]", 9.0);
}

#[test]
fn test_sort_descending() {
    run("3d10k1d1sd", "[9, 5d, 3d]", 9.0);
}

#[test]
fn test_negative_dice_sides() {
    let result = parse("d-15");
    assert!(result.is_err(), "{:?}", result);
}

#[test]
fn test_negative_expression_sides() {
    // Expressions leading to negative values are not evaluated until you try to roll dices.
    // if a negative value is resolved it should be set to 1.

    let result = parse("d(1 - 3)");
    assert!(result.is_err().not(), "{:?}", result);

    let rolled = result.unwrap().roll(&mut test_rng());
    assert_eq!(rolled.value(), 1.0);
}
