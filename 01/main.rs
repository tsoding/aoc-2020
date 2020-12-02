// Part 1
// O(N²)
#[allow(dead_code)]
fn part_1_n_squared(xs: Vec<i32>) {
    let n = xs.len();
    for i in 0..n - 1 {
        for j in i + 1..n {
            if xs[i] + xs[j] == 2020 {
                println!("{}", xs[i] * xs[j]);
                return;
            }
        }
    }
}

// Part 2
// O(N³)
#[allow(dead_code)]
fn part_2_n_cubed(xs: Vec<i32>) {
    let n = xs.len();
    for i in 0..n - 2 {
        for j in i + 1..n - 1 {
            for k in j + 1..n {
                if xs[i] + xs[j] + xs[k] == 2020 {
                    println!("{}", xs[i] * xs[j] * xs[k]);
                    return;
                }
            }
        }
    }
}

// Part 1
// O(NlogN)
#[allow(dead_code)]
fn part_1_n_logn(mut xs: Vec<i32>) {
    let n = xs.len();
    xs.sort();
    for i in 0..n {
        if let Ok(j) = xs.binary_search(&(2020 - xs[i])) {
            if i != j {
                println!("{}", xs[i] * xs[j]);
                return;
            }
        }
    }
}

// Part 2
// O(N²logN)
#[allow(dead_code)]
fn part_2_n_squared_logn(mut xs: Vec<i32>) {
    let n = xs.len();
    xs.sort();
    for i in 0..n - 1 {
        for j in i + 1..n {
            if let Ok(k) = xs.binary_search(&(2020 - xs[i] - xs[j])) {
                if j != k {
                    println!("{}", xs[i] * xs[j] * xs[k]);
                    return;
                }
            }
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let xs =
        input
        .split('\n')
        .filter(|s| !s.is_empty())
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    part_1_n_squared(xs);
    // part_2_n_cubed(xs);
    // part_1_n_logn(xs);
    // part_2_n_squared_logn(xs);
}
