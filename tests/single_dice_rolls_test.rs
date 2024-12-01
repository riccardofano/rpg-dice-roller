use rand::{rngs::StdRng, SeedableRng};
use rpg_dice_roller::roll_with;

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
    run("5d6", "[5, 6, 5, 5, 2]", 23.0);
}

#[test]
fn test_min_35() {
    run(
        "10d%min35",
        "[83, 98, 80, 70, 35^, 43, 35^, 35^, 78, 35^]",
        592.0,
    );
}

#[test]
fn test_min_last_applied() {
    run(
        "10d%min50min35",
        "[83, 98, 80, 70, 35^, 43, 35^, 35^, 78, 35^]",
        592.0,
    );
    run(
        "10d%min35min50",
        "[83, 98, 80, 70, 50^, 50^, 50^, 50^, 78, 50^]",
        659.0,
    );
}

#[test]
fn test_max() {
    run(
        "10d%max80",
        "[80v, 80v, 80, 70, 20, 43, 22, 18, 78, 26]",
        517.0,
    );
}

#[test]
fn test_max_last_applied() {
    run(
        "10d%max10max80",
        "[80v, 80v, 80, 70, 20, 43, 22, 18, 78, 26]",
        517.0,
    );
}

#[test]
fn test_min_and_max() {
    run(
        "10d%min35max80",
        "[80v, 80v, 80, 70, 35^, 43, 35^, 35^, 78, 35^]",
        571.0,
    );
}

#[test]
fn test_exploding_standard() {
    run("d4!", "[4!, 4!, 4!, 3]", 15.0);
}
#[test]
fn test_exploding_standard_cmp() {
    run(
        "d%!>10",
        "[83!, 98!, 80!, 70!, 20!, 43!, 22!, 18!, 78!, 26!, 52!, 97!, 80!, 39!, 100!, 71!, 12!, 53!, 16!, 39!, 55!, 21!, 42!, 7]",
        1222.0
    );
}

#[test]
fn test_exploding_penetrating() {
    run(
        "10d4!p",
        "[4!p, 3, 4!p, 2, 1, 2, 1, 1, 4!p, 1, 3, 4!p, 3, 2]",
        35.0,
    );
}

#[test]
fn test_exploding_penetrating_cmp() {
    run(
        "10d%!p<50",
        "[83, 98, 80, 70, 20!p, 42!p, 21!p, 17!p, 77, 26!p, 51, 97, 80, 39!p, 99, 71]",
        971.0,
    );
}

#[test]
fn test_exploding_compounding() {
    run("10d4!!", "[15!!, 1, 2, 1, 1, 6!!, 3, 10!!, 7!!, 1]", 47.0);
}

#[test]
fn test_exploding_compounding_cmp() {
    run(
        "10d%!!<=49",
        "[83, 98, 80, 70, 181!!, 78!!, 97, 80, 139!!, 71]",
        977.0,
    );
}

#[test]
fn test_exploding_penetrating_compounding() {
    run(
        "10d4!!p",
        "[7!!p, 6!!p, 1, 2, 1, 1, 5!!p, 3, 7!!p, 2]",
        35.0,
    );
}

#[test]
fn test_exploding_penetrating_compounding_cmp() {
    run(
        "10d%!!p=80",
        "[83, 98, 149!!p, 20, 43, 22, 18, 78, 26, 52]",
        589.0,
    );
}

#[test]
fn test_reroll() {
    run("10d4r", "[4, 4, 4, 3, 2r, 4r, 2, 3, 4, 4]", 34.0);
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
    run("10d4u", "[4, 3u, 1, 2, 3u, 3u, 3u, 2u, 1u, 3u]", 25.0);
}

#[test]
fn test_unique_once() {
    run("10d4uo", "[4, 4uo, 3, 1, 2, 1uo, 2uo, 4uo, 2uo, 3uo]", 26.0);
}

#[test]
fn test_unique_cmp() {
    run("10d4u>2", "[4, 3u, 1, 2, 1, 1, 2u, 2u, 1u, 1u]", 18.0);
}

#[test]
fn test_unique_once_cmp() {
    run("10d4uo<3", "[4, 4, 4, 3, 1, 2, 1uo, 4, 3uo, 4]", 30.0);
}

#[test]
fn test_target_success() {
    run(
        "14d4>3",
        "[4*, 4*, 4*, 3, 1, 2, 1, 1, 4*, 2, 3, 4*, 4*, 2]",
        6.0,
    );
}

#[test]
fn test_target_failure() {
    run(
        "14d4>3f=1",
        "[4*, 4*, 4*, 3, 1_, 2, 1_, 1_, 4*, 2, 3, 4*, 4*, 2]",
        3.0,
    );
}

#[test]
fn test_critical_success_cmp() {
    run(
        "14d4cs>2",
        "[4**, 4**, 4**, 3**, 1, 2, 1, 1, 4**, 2, 3**, 4**, 4**, 2]",
        39.0,
    );
}

#[test]
fn test_critical_success() {
    run(
        "14d4cs",
        "[4**, 4**, 4**, 3, 1, 2, 1, 1, 4**, 2, 3, 4**, 4**, 2]",
        39.0,
    );
}

#[test]
fn test_critical_failure() {
    run(
        "14d4cf",
        "[4, 4, 4, 3, 1__, 2, 1__, 1__, 4, 2, 3, 4, 4, 2]",
        39.0,
    );
}

#[test]
fn test_critical_failure_cmp() {
    run(
        "14d4cf<3",
        "[4, 4, 4, 3, 1__, 2__, 1__, 1__, 4, 2__, 3, 4, 4, 2__]",
        39.0,
    );
}

#[test]
fn test_keep() {
    run("4d50k1", "[42d, 49, 40d, 35d]", 49.0);
}

#[test]
fn test_keep_highest() {
    run("4d50kh1", "[42d, 49, 40d, 35d]", 49.0);
}

#[test]
fn test_keep_lowest() {
    run("4d50kl1", "[42d, 49d, 40d, 35]", 35.0);
}

#[test]
fn test_keep_highest_three() {
    run("4d50k3", "[42, 49, 40, 35d]", 131.0);
}

#[test]
fn test_keep_lowest_two() {
    run("4d50kl2", "[42d, 49d, 40, 35]", 75.0);
}

#[test]
fn test_drop() {
    run("4d50d1", "[42, 49, 40, 35d]", 131.0);
}

#[test]
fn test_drop_highest() {
    run("4d50dh1", "[42, 49d, 40, 35]", 117.0);
}

#[test]
fn test_drop_lowest() {
    run("4d50dl1", "[42, 49, 40, 35d]", 131.0);
}

#[test]
fn test_drop_highest_three() {
    run("4d50dh3", "[42d, 49d, 40d, 35]", 35.0);
}

#[test]
fn test_drop_lowest_two() {
    run("4d50dl2", "[42, 49, 40d, 35d]", 91.0);
}

#[test]
fn test_keep_drop() {
    run("3d10k1d1", "[9d, 10, 8d]", 10.0);
}

#[test]
fn test_sort() {
    run("3d10k1d1s", "[8d, 9d, 10]", 10.0);
}

#[test]
fn test_sort_ascending() {
    run("3d10k1d1sa", "[8d, 9d, 10]", 10.0);
}

#[test]
fn test_sort_descending() {
    run("3d10k1d1sd", "[10, 9d, 8d]", 10.0);
}
