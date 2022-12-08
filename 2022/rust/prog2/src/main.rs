use std::{io, io::prelude::*, collections::BTreeMap};

#[derive(Copy, Clone)]
enum Move { Rock, Paper, Scissors }
static ALL_MOVES:[Move; 3] = [Move::Rock, Move::Paper, Move::Scissors];

#[derive(Copy, Clone)]
enum RoundEnd { Win, Lose, Draw }

impl Move {
    fn eq(&self, other:&Move) -> bool {
        match(self, other) {
            (Move::Rock, Move::Rock) => true,
            (Move::Paper, Move::Paper) => true,
            (Move::Scissors, Move::Scissors) => true,
            _ => false,
        }
    }
    fn kills(&self, other:&Move) -> bool {
        match (self,other) {
        (Move::Rock, Move::Scissors) => true,
        (Move::Paper, Move::Rock) => true,
        (Move::Scissors, Move::Paper) => true,
        _ => false,
        }
    }
}

fn find_move(other:Move, re:RoundEnd) -> Move {
    match (other, re) {
    (m, RoundEnd::Draw) => m,
    (m, RoundEnd::Win) => *ALL_MOVES.iter().reduce(|acc:&Move, x: &Move| -> &Move {
        match x.kills(&m) {
            true => x,
            false => acc,
        }
    }).unwrap(),
    (m, RoundEnd::Lose) => *ALL_MOVES.iter().reduce(|acc:&Move, x: &Move| -> &Move {
        match m.kills(&x) {
            true => x,
            false => acc,
        }
    }).unwrap(),
    }
}

fn main() {
    let move_lut = BTreeMap::from([
        ('A' as u8, &Move::Rock), ('B' as u8, &Move::Paper), ('C' as u8, &Move::Scissors),
        ('X' as u8, &Move::Rock), ('Y' as u8, &Move::Paper), ('Z' as u8, &Move::Scissors)
    ]);
    let re_lut = BTreeMap::from([
        ('X' as u8, &RoundEnd::Lose), ('Y' as u8, &RoundEnd::Draw), ('Z' as u8, &RoundEnd::Win)
    ]);
    let score = |other:&Move, my:&Move| -> i32 {
        let win_points = match [other, my] {
        [other, my] if my.eq(other) => 3,
        [other, my ] if my.kills(other) => 6,
        _ => 0,
        };

        win_points + match my {
        Move::Rock => 1,
        Move::Paper => 2,
        Move::Scissors => 3,
        }
     };

    let lines = io::stdin().lock().lines();

    let (total1,total2) = lines.fold((0,0),|acc, line| {
        let b = line.unwrap().as_bytes().to_owned();
        if b.len() < 3 {
            return acc;
        }

        let moves = [b[0],b[2]].map(|x| move_lut[&x]);    
        let my = find_move(*moves[0], *re_lut[&b[2]]);

        (acc.0 + score(moves[0], moves[1]), acc.1 +score(moves[0], &my))
    });

    println!("1st total: {}", total1);
    println!("2nd total: {}", total2);
}
