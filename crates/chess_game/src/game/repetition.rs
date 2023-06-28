fn all_equal_and_theres_three<T>(mut it: impl Iterator<Item = T>) -> bool
where
    T: Eq,
{
    let Some(first) = it.next() else { return false; };
    let Some(second) = it.next() else { return false; };
    let Some(third) = it.next() else { return false; };

    first == second && second == third
}

pub fn repetition<T>(arr: &[T]) -> Option<&[T]>
where
    T: Eq,
{
    if arr.len() < 3 {
        return None;
    }

    let n = arr.len();
    for step in 1..(n / 3 + 1) {
        if !all_equal_and_theres_three(arr.iter().rev().step_by(step)) {
            continue;
        }
        let last_idx = n - 1;
        let second = last_idx - step;
        let first = last_idx - 2 * step;

        if first < step - 1 {
            return None;
        }

        if (1..step)
            .map(|i| (&arr[last_idx - i], &arr[second - i]))
            .all(|(a, b)| a == b)
        {
            return Some(&arr[first..]);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! rep_test {
        ($name:ident, $arr:expr) => {
            #[test]
            fn $name() {
                assert_eq!(repetition::<i32>(&$arr), None)
            }
        };

        ($name:ident, $arr:expr, $value:expr) => {
            #[test]
            fn $name() {
                assert_eq!(repetition::<i32>(&$arr), Some($value.as_slice()))
            }
        };
    }

    // positive cases
    rep_test!(only_repetiton, [1, 1, 1], [1, 1, 1]);
    rep_test!(different_repetitions, [2, 2, 2, 2, 1, 1, 1], [1, 1, 1]);
    rep_test!(stride2, [1, 2, 1, 2, 1, 2], [2, 1, 2, 1, 2]);
    rep_test!(stride3, [1, 2, 3, 1, 2, 3, 1, 2, 3], [3, 1, 2, 3, 1, 2, 3]);
    rep_test!(fence_post, [3, 3, 3, 0, 1, 0, 1, 0], [0, 1, 0, 1, 0]);
    rep_test!(
        confusing,
        [1, 1, 0, 1, 0, 0, 1, 0, 0],
        [0, 1, 0, 0, 1, 0, 0]
    );

    // positive cases
    rep_test!(empty, []);
    rep_test!(at_least_three, [1, 1]);
    rep_test!(must_repeat, [1, 2, 3]);
    rep_test!(must_repeat_threefold, [1, 1, 2, 2, 3, 3]);
    rep_test!(must_repeat_at_end, [1, 1, 1, 2]);
    rep_test!(stride2_but_wrong, [1, 2, 3, 2, 1, 2]);
    rep_test!(
        incomplete_stride3_too_little_data,
        [/*1, */ 2, 3, 1, 2, 3, 1, 2, 3]
    );
}
