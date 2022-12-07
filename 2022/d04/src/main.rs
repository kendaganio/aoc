use std::fs;

#[derive(Debug)]
struct Range {
    start: i32,
    end: i32,
}

impl Range {
    pub fn parse(str: &str) -> Self {
        let mut pair = str.split("-");

        Self {
            start: str::parse::<i32>(pair.next().unwrap()).unwrap(),
            end: str::parse::<i32>(pair.next().unwrap()).unwrap(),
        }
    }

    pub fn is_inside(&self, other: &Range) -> bool {
        self.start >= other.start && self.end <= other.end
    }

    pub fn overlaps(&self, other: &Range) -> bool {
        self.start <= other.end && self.end >= other.start
    }
}

fn main() {
    let input = fs::read_to_string("./src/in.txt").expect("!!");
    let lines = input.trim_end().split("\n");

    let ranges_in_pairs = lines
        .map(|line| {
            let mut pairs = line.split(",");
            let first = pairs.next().unwrap();
            let second = pairs.next().unwrap();
            return (Range::parse(first), Range::parse(second));
        })
        .collect::<Vec<(Range, Range)>>();

    let inside: usize = ranges_in_pairs
        .iter()
        .filter(|p| p.1.is_inside(&p.0) || p.0.is_inside(&p.1))
        .count();

    let overlaps: usize = ranges_in_pairs
        .iter()
        .filter(|p| p.1.overlaps(&p.0) || p.0.overlaps(&p.1))
        .count();

    println!("Part 1: {}", inside);
    println!("Part 2: {}", overlaps);
}
