
import { lines } from './input.mjs';

(async () => {
    class Field {
        constructor(trackdiags) {
            this.m = {};
            this.overlaps = 0;
            this.trackdiags = trackdiags;
        }

        track(x,y) {
            const key = `${x}~${y}`;
            if (this.m[key] === 1) this.overlaps++;
            this.m[key] = this.m[key] === undefined ? 1 : this.m[key] + 1;
        }

        trackLine(x1,y1,x2,y2) {
            let xdir = Math.sign(x2-x1), ydir = Math.sign(y2 - y1);
            if (!this.trackdiags && (xdir !== 0 && ydir !== 0)) return;
            for (let y = y1, x = x1; y !== y2+ydir || x !== x2+xdir; y+=ydir, x+=xdir) {
                this.track(x,y);
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


