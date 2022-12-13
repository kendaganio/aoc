use std::fs;
use std::time::Instant;

#[derive(Debug, Clone)]
pub struct File {
    name: String,
    is_dir: bool,
    size: i32,
}

pub fn is_numeric(s: &str) -> bool {
    s.chars().all(|c| c.is_numeric())
}

pub fn children(dir: &Vec<File>, path: &str) -> Vec<File> {
    dir.iter()
        .filter(|f| f.name.starts_with(&format!("{}/", path)) && !f.is_dir)
        .cloned()
        .collect::<Vec<File>>()
}

pub fn dir_size(dir: &Vec<File>, path: &str) -> i32 {
    let children = children(dir, path);
    children.iter().map(|c| c.size).sum()
}

fn main() {
    let now = Instant::now();
    let input = fs::read_to_string("./src/in.txt").expect("!");
    let mut lines = input.lines();

    let mut paths: Vec<String> = Vec::new();
    let mut files: Vec<File> = Vec::new();

    while let Some(line) = lines.next() {
        let split: Vec<&str> = line.split(" ").collect();
        if line.starts_with("$") {
            match split.get(1).unwrap().as_ref() {
                "cd" => {
                    let dir = split.get(2).unwrap();
                    if *dir == ".." {
                        paths.pop();
                    } else {
                        paths.push(dir.to_string());
                        files.push(File {
                            name: paths.join("/").to_string(),
                            is_dir: true,
                            size: 0,
                        });
                    }
                }
                _ => (),
            };
        } else if is_numeric(split.get(0).unwrap()) {
            let name = split.get(1).unwrap();
            let path = format!("{}/{}", paths.join("/").to_string(), &name);
            files.push(File {
                name: path.to_string(),
                is_dir: false,
                size: split.get(0).unwrap().parse().unwrap(),
            });
        }
    }

    let dirs: Vec<File> = files.iter().filter(|f| f.is_dir).cloned().collect();
    let free_space = 70_000_000 - dir_size(&files, "/");
    let min_size = 30_000_000 - free_space;

    let less_than_100k: i32 = dirs
        .iter()
        .map(|d| dir_size(&files, &d.name))
        .filter(|d| d <= &100_000)
        .sum();

    let deletable: i32 = dirs
        .iter()
        .map(|d| dir_size(&files, &d.name))
        .filter(|d| d >= &min_size)
        .min()
        .unwrap();

    println!("Part 1: {} | t: {:.2?}", &less_than_100k, now.elapsed());
    println!("Part 2: {} | t: {:.2?}", &deletable, now.elapsed());
}
