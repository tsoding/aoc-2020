fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let mut xs =
        input
        .split('\n')
        .filter(|s| !s.is_empty())
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    let n = xs.len();

    // // Part 1
    // // O(N²)
    // for i in 0..n - 1 {
    //     for j in i + 1..n {
    //         if xs[i] + xs[j] == 2020 {
    //             println!("{}", xs[i] * xs[j]);
    //             return;
    //         }
    //     }
    // }

    // // Part 2
    // // O(N³)
    // for i in 0..n - 2 {
    //     for j in i + 1..n - 1 {
    //         for k in j + 1..n {
    //             if xs[i] + xs[j] + xs[k] == 2020 {
    //                 println!("{}", xs[i] * xs[j] * xs[k]);
    //                 return;
    //             }
    //         }
    //     }
    // }

    // // Part 1
    // // O(NlogN)
    // xs.sort();
    // for i in 0..n {
    //     if let Ok(j) = xs.binary_search(&(2020 - xs[i])) {
    //         if i != j {
    //             println!("{}", xs[i] * xs[j]);
    //             return;
    //         }
    //     }
    // }

    // // Part 2
    // // O(N²logN)
    // xs.sort();
    // for i in 0..n - 1 {
    //     for j in i + 1..n {
    //         if let Ok(k) = xs.binary_search(&(2020 - xs[i] - xs[j])) {
    //             if j != k {
    //                 println!("{}", xs[i] * xs[j] * xs[k]);
    //                 return;
    //             }
    //         }
    //     }
    // }

    // TODO: O(N)
    // TODO: O(N²)
}
