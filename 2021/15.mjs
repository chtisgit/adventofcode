
import { lines } from './input.mjs';

function* coords(width, height) {
    for (let y = 0; y !== height; y++)
        for (let x = 0; x !== width; x++)
            yield {x,y};
}

const shortest = (target, pred) => {
    const path = [target];
    while (pred[target.y][target.x] !== null) {
        target = pred[target.y][target.x];
        path.unshift(target);
    }
    return path;
}

const arr2D = (w,h,val) => Array.from({length:h}, () => Array.from({length:w}, () => val));

const dijkstra = (map, source, target, risk) => {
    const w = map[0].length, h = map.length;
    const Q = [...coords(w,h)];
    const neighbors = ({x,y}) => 
        [{x:x,y:y-1},{x:x-1,y:y},{x:x+1,y:y},{x:x,y:y+1}]
        .filter((pos) => risk(pos) < 10);
    const dist = arr2D(w,h,Infinity)
    const pred = arr2D(w,h,null);
    
    dist[source.y][source.x] = 0;

    const done = Array.from({length:w*h}, () => false);
    
    while(Q.length !== 0) {
        const u = Q.reduce((acc, {x,y}) => acc === null || dist[y][x] < dist[acc.y][acc.x] ? {x,y} : acc, null);
        const ri = Q.findIndex(({x,y}) => x === u.x && y === u.y);
        Q.splice(ri, 1);
        done[u.x+u.y*w] = true;
    
        for (let v of neighbors(u)) {
            if (!done[v.x+v.y*w]) {
                const alt = dist[u.y][u.x] + risk(v);
                if (alt < dist[v.y][v.x]) {
                    dist[v.y][v.x] = alt;
                    pred[v.y][v.x] = u;
                }
            }
        }
        console.log('Q.length: ', Q.length);
    }

    return shortest(target, pred);
}

const fullmap = (map, w, h) => {
    const nmap = [];
    for (let y = 0; y !== 5*h; y++) {
        let s = '';
        for (let x = 0; x !== 5*w; x++) {
            const incr = parseInt(x/w)+parseInt(y/w);
            let n = +map[y % h][x % w] + incr;
            if (n > 9) n -= 9;
            s += n;
        }
        nmap.push(s);
    }
    return nmap;
}

lines().then((data) => {
    const w = data[0].length, h = data.length;
    const risk = ({x,y}) => data[y]?.[x] ? +data[y][x] : 10;
    const part1 = dijkstra(data, {x:0,y:0}, {x:w-1,y:h-1}, risk).map((pos) => risk(pos)).reduce((acc,val) => acc+val, 0) - risk({x:0,y:0});
    console.log('part 1:', part1);
    
    const data2 = fullmap(data, w, h);
    const risk2 = ({x,y}) => data2[y]?.[x] ? +data2[y][x] : 10;
    const part2 = dijkstra(data2, {x:0,y:0}, {x:5*w-1,y:5*h-1}, risk2).map((pos) => risk2(pos)).reduce((acc,val) => acc+val, 0) - risk({x:0,y:0});
    console.log('part 2:', part2);
});
