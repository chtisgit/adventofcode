use std::mem::swap;
use std::{io, io::prelude::*};

struct ElfIterator<T> {
    cnt: i32,
    lines: T,
    cur_vec: Vec<i32>,
}

impl<T: Iterator<Item = io::Result<String>>> Iterator for ElfIterator<T> {
    type Item = (i32, Vec<i32>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let res = self.lines.next();

            match res {
                None => return None,
                Some(Err(_)) => {
                    println!("error!");
                    return None;
                }
                Some(Ok(line)) => {
                    if let Ok(n) = line.parse::<i32>() {
                        self.cur_vec.push(n);
                    } else {
                        let cnt = self.cnt;
                        self.cnt += 1;

                        let mut v = Vec::new();
                        swap(&mut v, &mut self.cur_vec);
                        return Some((cnt, v));
                    }
                }
            }
        }
    }
}

fn new_elf_iterator<T>(lines: T) -> ElfIterator<T> {
    ElfIterator {
        lines: lines,
        cnt: 1,
        cur_vec: Vec::new(),
    }
}

struct Top3 {
    top3: [Option<(i32, i32)>; 3],
}

impl Top3 {
    fn add(&mut self, tup: (i32, i32)) {
        let (_, cal) = tup;
        if cal > self.top3[0].unwrap_or((0, 0)).1 {
            self.top3[2] = self.top3[1];
            self.top3[1] = self.top3[0];
            self.top3[0] = Some(tup);
        } else if cal > self.top3[1].unwrap_or((0, 0)).1 {
            self.top3[2] = self.top3[1];
            self.top3[1] = Some(tup);
        } else if cal > self.top3[2].unwrap_or((0, 0)).1 {
            self.top3[2] = Some(tup);
        }
    }
}

fn main() {
    let mut result = Top3 {
        top3: [None, None, None],
    };
    new_elf_iterator(io::stdin().lock().lines())
        .map(|x| -> (i32, i32) {
            let (elf, calories) = x;
            (elf, calories.iter().sum())
        })
        .for_each(|tup| {
            result.add(tup);
        });

    match result.top3[0] {
        Some((elf, cal)) => println!("Elf {} trägt am meisten ({} Kalorien).", elf, cal),
        _ => println!("error!"),
    }

    let top3sum = result
        .top3
        .iter()
        .fold(0, |acc, tup| -> i32 { tup.unwrap().1 + acc });
    println!("Die Top 3 trägt zusammen: {} Kalorien", top3sum);
}
