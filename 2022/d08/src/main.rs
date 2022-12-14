use std::collections::HashMap;
use std::fs;
use std::time::Instant;

pub fn is_visible(
    trees: &HashMap<(usize, usize), u32>,
    at: &(usize, usize),
    rows: usize,
    cols: usize,
) -> bool {
    let initial = at.clone();
    let og = trees[&initial];
    let (mut x, mut y) = initial;

    y += 1;
    while let Some(new_value) = trees.get(&(x, y)) {
        if og > *new_value {
            if y == cols - 1 {
                return true;
            }
            y += 1;
        } else {
            break;
        }
    }

    y = initial.1;
    x = initial.0 + 1;
    while let Some(new_value) = trees.get(&(x, y)) {
        if og > *new_value {
            if x == rows - 1 {
                return true;
            }
            x += 1;
        } else {
            break;
        }
    }

    y = initial.1 - 1;
    x = initial.0;
    while let Some(new_value) = trees.get(&(x, y)) {
        if og > *new_value {
            if y == 0 {
                return true;
            }
            y -= 1;
        } else {
            break;
        }
    }

    y = initial.1;
    x = initial.0 - 1;
    while let Some(new_value) = trees.get(&(x, y)) {
        if og > *new_value {
            if x == 0 {
                return true;
            }
            x -= 1;
        } else {
            break;
        }
    }

    false
}

pub fn scenic_score(trees: &HashMap<(usize, usize), u32>, at: &(usize, usize)) -> i32 {
    let initial = at.clone();
    let og = trees[&initial];
    let (mut x, mut y) = initial;

    let mut u = 0;
    let mut d = 0;
    let mut r = 0;
    let mut l = 0;

    y += 1;
    while let Some(new_value) = trees.get(&(x, y)) {
        r += 1;
        if *new_value >= og {
            break;
        }
        y += 1;
    }

    y = initial.1;
    x = initial.0 + 1;
    while let Some(new_value) = trees.get(&(x, y)) {
        d += 1;
        if *new_value >= og {
            break;
        }
        x += 1;
    }

    y = initial.1 - 1;
    x = initial.0;
    while let Some(new_value) = trees.get(&(x, y)) {
        l += 1;
        if *new_value >= og || y == 0 {
            break;
        }
        y -= 1;
    }

    y = initial.1;
    x = initial.0 - 1;
    while let Some(new_value) = trees.get(&(x, y)) {
        u += 1;
        if *new_value >= og || x == 0 {
            break;
        }
        x -= 1;
    }

    u * l * r * d
}

fn main() {
    let mut trees: HashMap<(usize, usize), u32> = HashMap::new();
    let mut visible: i32 = 0;
    let mut most_scenic: i32 = 0;
    let now = Instant::now();
    let input = fs::read_to_string("./src/in.txt").expect("!");
    let lines = input.trim_end().lines();
    let rows = lines.clone().count();
    let cols = lines.clone().last().unwrap().chars().count();

    for (r, line) in input.lines().enumerate() {
        for (c, h) in line.chars().enumerate() {
            trees.insert((r, c), h.to_digit(10).unwrap());
        }
    }

    for i in 0..rows {
        for j in 0..cols {
            if i == 0 || j == 0 || i == rows - 1 || j == cols - 1 {
                visible += 1;
                continue;
            }
            if is_visible(&trees, &(i, j), rows, cols) {
                visible += 1;
            }

            let score = scenic_score(&trees, &(i, j));
            if score > most_scenic {
                most_scenic = score;
            }
        }
    }

    println!("Part 1: {} | t: {:.2?}", visible, now.elapsed());
    println!("Part 2: {} | t: {:.2?}", most_scenic, now.elapsed());
}
