use std::time::Instant;
use std::{cmp, collections::HashMap, fs};

#[derive(Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug)]
pub struct Action {
    direction: Direction,
    count: i32,
}

impl Action {
    pub fn parse(line: &str) -> Self {
        let split: Vec<&str> = line.split(" ").collect();
        let dir = split.get(0).unwrap().chars().nth(0).unwrap();
        let count: i32 = split.get(1).unwrap().parse().unwrap();
        match dir {
            'U' => Self {
                direction: Direction::Up,
                count,
            },
            'D' => Self {
                direction: Direction::Down,
                count,
            },
            'L' => Self {
                direction: Direction::Left,
                count,
            },
            'R' => Self {
                direction: Direction::Right,
                count,
            },
            _ => unreachable!(),
        }
    }
}

pub fn move_head(action: &Action, head: &(i32, i32)) -> (i32, i32) {
    let mut new_head = head.to_owned();
    match &action.direction {
        Direction::Up => new_head.0 -= 1,
        Direction::Down => new_head.0 += 1,
        Direction::Left => new_head.1 -= 1,
        Direction::Right => new_head.1 += 1,
    }
    new_head
}

pub fn move_tail(head: &(i32, i32), tail: &(i32, i32)) -> (i32, i32) {
    let dx = head.0 - tail.0;
    let dy = head.1 - tail.1;

    match cmp::max(dx.abs(), dy.abs()) {
        1 => tail.to_owned(),
        _ => (tail.0 + dx.signum(), tail.1 + dy.signum()),
    }
}

pub fn solve_part1(actions: &Vec<Action>) -> i32 {
    let mut visited: HashMap<(i32, i32), bool> = HashMap::new();
    let mut head = (0, 0);
    let mut tail = (0, 0);

    visited.insert(tail, true);
    actions.iter().for_each(|a| {
        for _ in 0..a.count {
            head = move_head(&a, &head);
            tail = move_tail(&head, &tail);
            visited.insert(tail, true);
        }
    });

    visited.len() as i32
}

pub fn solve_part2(actions: &Vec<Action>) -> i32 {
    let mut visited: HashMap<(i32, i32), bool> = HashMap::new();
    let mut tails = vec![(0, 0); 10];

    actions.iter().for_each(|a| {
        for _ in 0..a.count {
            tails[0] = move_head(&a, &tails[0]);
            for t_idx in 1..tails.len() {
                tails[t_idx] = move_tail(&tails[t_idx - 1], &tails[t_idx]);
            }
            visited.insert(tails[9], true);
        }
    });

    visited.len() as i32
}

fn main() {
    let input = fs::read_to_string("./src/in.txt").expect("!");
    let directions: Vec<Action> = input.trim_end().lines().map(Action::parse).collect();

    let mut now = Instant::now();
    println!(
        "Part 1: {} | t: {:.2?}",
        solve_part1(&directions),
        now.elapsed()
    );

    now = Instant::now();
    println!(
        "Part 2: {} | t: {:.2?}",
        solve_part2(&directions),
        now.elapsed()
    );
}
