use std::collections::HashMap;
use std::fs;
use std::time::Instant;

#[derive(Debug)]
pub struct Command {
    amount: usize,
    from: usize,
    to: usize,
}

pub fn stringify(stacks: &HashMap<usize, Vec<char>>) -> String {
    let mut own_stacks = stacks.to_owned();
    let mut out = String::new();
    for i in 1..own_stacks.keys().len() + 1 {
        let s = own_stacks.get_mut(&i).expect("!");
        if !s.is_empty() {
            out.push(s.pop().unwrap());
        }
    }
    out
}

pub fn solve_part1(stacks: &HashMap<usize, Vec<char>>, commands: &Vec<Command>) -> String {
    let mut own_stacks = stacks.to_owned();

    for command in commands {
        for _ in 0..command.amount {
            let from = own_stacks.get_mut(&command.from).expect("!");
            let item = from.pop().unwrap();
            let to = own_stacks.get_mut(&command.to).expect("!");
            to.push(item);
        }
    }

    stringify(&own_stacks)
}

pub fn solve_part2(stacks: &HashMap<usize, Vec<char>>, commands: &Vec<Command>) -> String {
    let mut own_stacks = stacks.to_owned();

    for command in commands {
        let mut to_move: Vec<char> = vec![];
        for _ in 0..command.amount {
            let from = own_stacks.get_mut(&command.from).expect("!");
            to_move.push(from.pop().unwrap());
        }
        let to = own_stacks.get_mut(&command.to).expect("!");
        to.extend(to_move.iter().rev());
    }

    stringify(&own_stacks)
}

fn main() {
    let now = Instant::now();
    let mut stacks: HashMap<usize, Vec<char>> = HashMap::new();
    let file = fs::read_to_string("./src/in.txt").expect("!!");
    let lines = file.lines();

    lines
        .to_owned()
        .take_while(|l| !l.is_empty())
        .for_each(|l| {
            for (i, chunk) in l.chars().collect::<Vec<char>>().chunks(4).enumerate() {
                let block = chunk.get(1).unwrap();
                if *block == ' ' || block.is_numeric() {
                    continue;
                }

                if !stacks.contains_key(&(i + 1)) {
                    stacks.insert(i + 1, vec![]);
                }

                stacks.get_mut(&(i + 1)).expect("woop!").insert(0, *block);
            }
        });

    let commands: Vec<Command> = lines
        .skip_while(|l| !l.contains("move"))
        .map(|c| {
            let split: Vec<&str> = c.split(" ").collect();
            Command {
                amount: str::parse::<usize>(split.get(1).unwrap()).unwrap(),
                from: str::parse::<usize>(split.get(3).unwrap()).unwrap(),
                to: str::parse::<usize>(split.get(5).unwrap()).unwrap(),
            }
        })
        .collect();

    println!(
        "Part 1: {} | t: {:.2?}",
        solve_part1(&stacks, &commands),
        now.elapsed()
    );
    println!(
        "Part 1: {} | t: {:.2?}",
        solve_part2(&stacks, &commands),
        now.elapsed()
    );
}
