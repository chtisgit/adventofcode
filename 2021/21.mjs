import { lines } from './input.mjs';

function* deterministicDie() {
    let rolled = 1;
    while(true)
        for (let n = 1; n <= 100; n++, rolled++)
            yield {n,rolled};
}

// 3 ... [1,1,1] (1)
// 4 ... [1,1,2] (3)
// 5 ... [1,2,2], [1,1,3] (6)
// 6 ... [1,2,3], [2,2,2] (7)
// 7 ... [1,3,3], [2,2,3] (6)
// 8 ... [2,3,3] (3)
// 9 ... [3,3,3] (1)
var cntm = [[3,1],[4,3],[5,6],[6,7],[7,6],[8,3],[9,1]];
const part2 = (players, i, u = 1, univ = [0,0], ind = 0) => {
    if (players[1-i].score >= 21) {
        univ[1-i] += u;
        return univ;
    }
    
    for (let [add, cnt] of cntm) {
        const newpos = (players[i].pos+add) %10;
        const newscore = (players[i].score + newpos + 1);
        const np = players.map(({pos, score},n) => (n === i ? {pos: newpos, score: newscore} : {pos,score}));
        part2(np, 1-i, u * cnt, univ, ind+1);
    }

    return univ;
}

const part1 = (players) => {
    let roll = deterministicDie();
    for (let i = 0; players[0].score < 1000 && players[1].score < 1000; i = 1-i) {
        const add = roll.next().value.n+roll.next().value.n+roll.next().value.n;
        players[i].pos = (players[i].pos + add) % 10;
        players[i].score += players[i].pos+1;
    }
    return Math.min(players[0].score,players[1].score)*(roll.next().value.rolled - 1);
}

lines().then((data) => {
    const players = data.map((line) => +/[0-9]+$/.exec(line))
        .filter((pos) => !isNaN(pos))
        .map((pos) => ({pos: pos-1, score: 0}));

    console.log('part 1: ', part1(JSON.parse(JSON.stringify(players))));

    const univ = part2(players, 0);
    console.log('part 2: ', Math.max(univ[0],univ[1]));
});
