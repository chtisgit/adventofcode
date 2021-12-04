
import { lines } from './input.mjs';

(async () => {
    class Board {
        constructor(nums, idx) {
            this.idx = idx;
            this.m = Array.from({length: 5}, (_, i) => Array.from({length: 5}, (_, j) => ({ n: nums[i*5+j], mark: false })));
        }
        mark(num,yes = true) { this.m.forEach((line) => line.forEach((o) => { if (num === o.n) o.mark = yes; })); }
        column(x) { return this.m.map((_,y) => this.m[y][x]); }
        columns() { return Array.from({length: 5}, (_, x) => this.column(x)); }
        win() {
            return this.m.some((line) => line.every((o) => o.mark)) ||
                this.columns().some((col) => col.every((o) => o.mark));
        }
        score(lastnum) { return lastnum * this.m.reduce((acc, line) => acc + line.reduce((acc, o) => (o.mark ? acc : acc+o.n), 0), 0); }
    }

    const data = (await lines()).map((line) => line.trim());

    const numbers = data[0].split(',').map((v) => +v);
    data.splice(0,1);

    const boardnumbers = data.join(' ').split(' ').map((v) => parseInt(v)).filter((v) => !isNaN(v) && typeof(v) === 'number');
    let boards = Array.from({ length: boardnumbers.length/25},
        (_, i) => boardnumbers.slice(25*i, 25*(i+1)))
        .map((nums, i) => new Board(nums,i+1));
    
        
    let cnt = 0;
    for (const num of numbers) {
        boards.forEach((board, i) => {
            board.mark(num);
            if (board.win()) {
                cnt++;
                if (cnt === 1) {
                    console.log(`part 1: Board ${board.idx} won. Score: ${board.score(num)}`);
                }
                if (boards.length === 1) {
                    const board = boards[0];
                    console.log(`part 2: Board ${board.idx} will win last. Score: ${board.score(num)}`);
                }
            }
        });

        boards = boards.filter((board) => !board.win());
    }
})();


