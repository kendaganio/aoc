use std::fs;

#[derive(Debug)]
pub enum Result {
    Win = 6,
    Draw = 3,
    Lose = 0,
    Noop = -1,
}

impl Result {
    pub fn parse(ch: &char) -> Self {
        match ch {
            'X' => Self::Lose,
            'Y' => Self::Draw,
            'Z' => Self::Win,
            _ => Self::Noop,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Hand {
    Rock = 1,
    Paper = 2,
    Scissors = 3,
    Lol = 0,
}

impl Hand {
    pub fn parse(ch: &char) -> Self {
        match ch {
            'A' | 'X' => Self::Rock,
            'B' | 'Y' => Self::Paper,
            'C' | 'Z' => Self::Scissors,
            _ => Self::Lol,
        }
    }

    pub fn fight(&self, other: &Hand) -> Result {
        match (self, other) {
            (Hand::Paper, Hand::Rock) => Result::Win,
            (Hand::Paper, Hand::Scissors) => Result::Lose,
            (Hand::Paper, Hand::Paper) => Result::Draw,

            (Hand::Rock, Hand::Rock) => Result::Draw,
            (Hand::Rock, Hand::Scissors) => Result::Win,
            (Hand::Rock, Hand::Paper) => Result::Lose,

            (Hand::Scissors, Hand::Rock) => Result::Lose,
            (Hand::Scissors, Hand::Scissors) => Result::Draw,
            (Hand::Scissors, Hand::Paper) => Result::Win,

            _ => Result::Lose,
        }
    }

    pub fn infer_move(&self, result: &Result) -> Self {
        match (self, result) {
            (Hand::Paper, Result::Win) => Hand::Scissors,
            (Hand::Paper, Result::Lose) => Hand::Rock,

            (Hand::Rock, Result::Win) => Hand::Paper,
            (Hand::Rock, Result::Lose) => Hand::Scissors,

            (Hand::Scissors, Result::Win) => Hand::Rock,
            (Hand::Scissors, Result::Lose) => Hand::Paper,

            _ => self.clone(),
        }
    }

    pub fn score(&self, other: &Hand) -> i32 {
        self.fight(other) as i32 + *self as i32
    }
}

pub fn solve_part1(pairs: &Vec<(Hand, Hand)>) -> i32 {
    pairs.iter().map(|pair| pair.1.score(&pair.0)).sum()
}

pub fn solve_part2(pairs: &Vec<(Hand, Result)>) -> i32 {
    pairs
        .iter()
        .map(|pair| {
            let chosen_hand = pair.0.infer_move(&pair.1);
            chosen_hand.score(&pair.0)
        })
        .sum()
}

fn main() {
    let input = fs::read_to_string("./src/in.txt").expect("no!");
    let pairs: Vec<(Hand, Hand)> = input
        .trim_end()
        .split("\n")
        .map(|pair| {
            (
                Hand::parse(&pair.chars().nth(0).unwrap()),
                Hand::parse(&pair.chars().nth(2).unwrap()),
            )
        })
        .collect();

    let hand_and_result: Vec<(Hand, Result)> = input
        .trim_end()
        .split("\n")
        .map(|pair| {
            (
                Hand::parse(&pair.chars().nth(0).unwrap()),
                Result::parse(&pair.chars().nth(2).unwrap()),
            )
        })
        .collect();

    println!("Part 1: {}", solve_part1(&pairs));
    println!("Part 2: {}", solve_part2(&hand_and_result))
}
