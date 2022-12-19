use std::fs;
use std::time::Instant;

#[derive(Debug)]
pub enum Instruction {
    Noop,
    Addx(i32),
}

impl Instruction {
    pub fn parse(line: &str) -> Self {
        let split: Vec<&str> = line.split(" ").collect();
        let cmd = split.get(0).unwrap();
        match *cmd {
            "noop" => Self::Noop,
            "addx" => Self::Addx(split.get(1).unwrap().parse().unwrap()),
            _ => unreachable!(),
        }
    }
}

pub fn solve_part2(instructions: &Vec<Instruction>) -> i32 {
    let mut x = 1;
    let mut cycle = 1;

    for i in instructions.iter() {
        if ((cycle % 40 - x - 1) as i32).abs() < 2 {
            print!("#")
        } else {
            print!(".")
        }

        if cycle % 40 == 0 {
            println!("");
        }

        if let Instruction::Addx(n) = i {
            cycle += 1;

            if ((cycle % 40 - x - 1) as i32).abs() < 2 {
                print!("#")
            } else {
                print!(".")
            }
            if cycle % 40 == 0 {
                println!("");
            }

            x += n;
        }

        cycle += 1;
    }

    0
}

pub fn solve_part1(instructions: &Vec<Instruction>) -> i32 {
    let mut x = 1;
    let mut cycle = 1;
    let mut strength = 0;

    for i in instructions.iter() {
        if cycle % 40 == 20 {
            strength += cycle * x;
        }

        if let Instruction::Addx(n) = i {
            cycle += 1;
            if cycle % 40 == 20 {
                strength += cycle * x;
            }
            x += n;
        }

        cycle += 1;
    }

    strength
}

fn main() {
    let now = Instant::now();
    let input = fs::read_to_string("./src/in.txt").expect("!");
    let instructions: Vec<Instruction> = input.trim_end().lines().map(Instruction::parse).collect();

    println!(
        "Part 1: {} | t: {:.2?}",
        solve_part1(&instructions),
        now.elapsed()
    );
    println!(
        "Part 2: {} | t: {:.2?}",
        solve_part2(&instructions),
        now.elapsed()
    );
}
