use std::collections::HashSet;
use std::fs;

pub fn priority(ch: &char) -> i32 {
    if ch.is_uppercase() {
        *ch as i32 - 38
    } else {
        *ch as i32 - 96
    }
}

pub fn solve_part1(bags: &Vec<(&str, &str)>) -> i32 {
    let mut totes = 0;

    for bag in bags {
        let a: HashSet<char> = bag.0.chars().collect();
        let b: HashSet<char> = bag.1.chars().collect();

        let mut i = a.intersection(&b);
        totes += priority(i.next().unwrap());
    }

    totes
}

pub fn solve_part2(bags: &Vec<&str>) -> i32 {
    let mut totes = 0;

    for chonk in bags.chunks(3) {
        let sets: Vec<HashSet<char>> = chonk
            .iter()
            .map(|bag| bag.chars().collect::<HashSet<char>>())
            .collect();

        if let [one, two, three] = sets.as_slice() {
            let a = one.intersection(&two).collect::<HashSet<&char>>();
            let b = two.intersection(&three).collect::<HashSet<&char>>();
            let mut c = a.intersection(&b);

            totes += priority(c.next().unwrap());
        }
    }

    totes
}
fn main() {
    let input = fs::read_to_string("./src/in.txt").expect("no!");
    let lines: Vec<&str> = input.trim_end().split("\n").collect();

    let bags: Vec<(&str, &str)> = lines
        .iter()
        .map(|line| line.split_at(line.len() / 2))
        .collect();

    println!("Part 1: {}", solve_part1(&bags));
    println!("Part 1: {}", solve_part2(&lines));
}
