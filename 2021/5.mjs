
import { lines } from './input.mjs';

(async () => {
    class Field {
        constructor(trackdiags) {
            this.m = {};
            this.overlaps = 0;
            this.trackdiags = trackdiags;
        }

        track(x,y) {
            if (!this.m[y]) {
                this.m[y] = {};
                this.m[y][x] = 1;
                return;
            }

            if (!this.m[y][x]) {
                this.m[y][x] = 1;
                return;
            }

            if (this.m[y][x] === 1) {
                this.overlaps++;
            }
            this.m[y][x]++;
        }

        trackLine(x1,y1,x2,y2) {
            if (y1 === y2) {
                let i1 = Math.min(x1,x2), i2 = Math.max(x1,x2);
                for (let x = i1; x <= i2; x++) {
                    this.track(x,y1);
                }
            } else if (x1 === x2) {
                let i1 = Math.min(y1,y2), i2 = Math.max(y1,y2);
                for (let y = i1; y <= i2; y++) {
                    this.track(x1,y);
                }
            } else if (this.trackdiags) {
                let xdir = x2 > x1 ? 1 : -1, ydir = y2 > y1 ? 1 : -1;
                for (let y = y1, x = x1; y !== y2+ydir && x !== x2+xdir; y+=ydir, x+=xdir) {
                    this.track(x,y);
                }
            }
        }
    }

    const data = (await lines())
        .map((line) => line.split('->').map((v) => v.trim().split(',').map((v) => +v)));

    const field = new Field(false);
    data.forEach((vec) => field.trackLine(vec[0][0],vec[0][1],vec[1][0],vec[1][1]));

    console.log(`part 1 : ${field.overlaps}`);


    const field2 = new Field(true);
    data.forEach((vec) => field2.trackLine(vec[0][0],vec[0][1],vec[1][0],vec[1][1]));

    console.log(`part 2 : ${field2.overlaps}`);

})();


