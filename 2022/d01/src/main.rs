use std::fs;
use std::time::Instant;

fn solve_part1(input: &Vec<i32>) -> i32 {
    *input.last().unwrap()
}

fn solve_part2(input: &Vec<i32>) -> i32 {
    input.iter().skip(input.len() - 3).sum()
}

fn main() {
    let now = Instant::now();
    let input = fs::read_to_string("./src/in.txt").expect("no!");
    let elves: Vec<Vec<i32>> = input
        .split("\n\n")
        .map(|elf| {
            elf.trim_end()
                .split("\n")
                .map(|n| n.parse::<i32>().unwrap())
                .collect()
        })
        .collect();

    let mut calories: Vec<i32> = elves.iter().map(|elf| elf.iter().sum()).collect();
    calories.sort_unstable();

    println!(
        "Part 1: {} | t: {:.2?}",
        solve_part1(&calories),
        now.elapsed()
    );

    println!(
        "Part 2: {} | t: {:.2?}",
        solve_part2(&calories),
        now.elapsed()
    );
}
