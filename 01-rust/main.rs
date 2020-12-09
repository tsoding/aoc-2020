// Part 1
// O(N²)
fn part_1_n_squared(xs: &Vec<i32>) -> i32 {
    let n = xs.len();
    for i in 0..n - 1 {
        for j in i + 1..n {
            if xs[i] + xs[j] == 2020 {
                return xs[i] * xs[j];
            }
        }
    }
    unreachable!();
}

// Part 2
// O(N³)
fn part_2_n_cubed(xs: &Vec<i32>) -> i32 {
    let n = xs.len();
    for i in 0..n - 2 {
        for j in i + 1..n - 1 {
            for k in j + 1..n {
                if xs[i] + xs[j] + xs[k] == 2020 {
                    return xs[i] * xs[j] * xs[k];
                }
            }
        }
    }
    unreachable!();
}

// Part 1
// O(NlogN)
fn part_1_n_logn(xs0: &Vec<i32>) -> i32 {
    let mut xs = xs0.clone();
    let n = xs.len();
    xs.sort();
    for i in 0..n {
        if let Ok(j) = xs.binary_search(&(2020 - xs[i])) {
            if i != j {
                return xs[i] * xs[j];
            }
        }
    }
    unreachable!();
}

// Part 2
// O(N²logN)
fn part_2_n_squared_logn(xs0: &Vec<i32>) -> i32 {
    let mut xs = xs0.clone();
    let n = xs.len();
    xs.sort();
    for i in 0..n - 1 {
        for j in i + 1..n {
            if let Ok(k) = xs.binary_search(&(2020 - xs[i] - xs[j])) {
                if j != k {
                    return xs[i] * xs[j] * xs[k];
                }
            }
        }
    }
    unreachable!();
}

fn main() {
    use std::env::args;
    use std::fs::read_to_string;

    let input_file = 
        args()
        .skip(1)
        .next()
        .expect("Input file is not provided");
    let input = read_to_string(&input_file).unwrap();
    let xs =
        input
        .split('\n')
        .filter(|s| !s.is_empty())
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    println!("Input file: {}", input_file);
    println!("Part 1: O(N²):     {}", part_1_n_squared(&xs));
    println!("Part 2: O(N³):     {}", part_2_n_cubed(&xs));
    println!("Part 1: O(NlogN):  {}", part_1_n_logn(&xs));
    println!("Part 2: O(N²logN): {}", part_2_n_squared_logn(&xs));
}
