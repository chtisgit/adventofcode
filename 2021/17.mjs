
import { lines } from './input.mjs';

lines().then((data) => {
    const match = /target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)/.exec(data)
    const tgt = match.slice(1,5).map((n) => parseInt(n));
    const within = ([x,y],[x1,x2,y1,y2]) => (x >= x1 && y >= y1 && x <= x2 && y <= y2);
    const beyond = ([x,y],[x1,x2,y1,y2]) => (x > x2 || y < y1);

    class Probe {
        constructor(pos, v) { this.pos = [pos[0],pos[1]]; this.v = v; this.traj = [this.pos]; }
        next() {
            this.pos = this.pos.map((_,i) => this.pos[i] + this.v[i]);
            this.v[0] -= Math.sign(this.v[0]);
            this.v[1] -= 1;
            this.traj.push(this.pos);
        }
    }

    const check = (pos, v, tgt) => {
        const p = new Probe(pos, v);
        while (!beyond(p.pos, tgt)) {
            p.next();
            if (within(p.pos, tgt)) return p;
        }
        return null;
    }
    
    let best = null;
    let velocities = [];
    for (let vx = 0; vx <= tgt[1]; vx++) {
        for (let vy = tgt[2]; vy <= tgt[1]; vy++) {
            const res = check([0,0], [vx,vy], tgt);
            if (res === null) continue;
            velocities.push([vx,vy]);

            if (vx < 100 && vy > 0 && vy < 100) {
                const highest = Math.max(...res.traj.map(([x,y]) => y));
                if (best === null || best.highest < highest)
                    best = {v: [vx,vy], traj: res.traj, highest};
            }
        }
    }

    console.log('part 1:',best.highest);
    console.log('part 2:',velocities.length);
});