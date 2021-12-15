
import { lines } from './input.mjs';

const arr2D = (w,h,val) => Array.from({length:h}, () => Array.from({length:w}, () => val));

const findIndex = (arr, val, lessThan) => {
    let low = 0, high = arr.length;
    while (low < high) {
        const mid = (low + high) >>> 1;
        if (lessThan(arr[mid], val)) {
            low = mid + 1;
        } else {
            high = mid
        }
    }
    return low;
};

const insertAt = (arr, val, lessThan) => {
    const position = findIndex(arr, val, lessThan);
    for (let i = position; i !== arr.length; i++){
        const tmp = arr[i];
        arr[i] = val;
        val = tmp;
    };
    arr.push(val);
};

const shortest = (target, pred) => {
    const path = [target];
    while (pred[target.y][target.x] !== null) {
        target = pred[target.y][target.x];
        path.unshift(target);
    }
    return path;
}

const dijkstra = (map, source, target, risk) => {
    const w = map[0].length, h = map.length;
    const neighbors = ({x,y}) => [{x:x,y:y-1},{x:x-1,y:y},{x:x+1,y:y},{x:x,y:y+1}].filter((pos) => risk(pos) < 10);
    const dist = arr2D(w,h,Infinity)
    const pred = arr2D(w,h,null);
	const wasinq = arr2D(w,h,false);

    const done = Array.from({length:w*h}, () => false);
    
    const Q = [source];
    dist[source.y][source.x] = 0;
    wasinq[source.y][source.x] = true;

    while(Q.length !== 0 && !done[target.x+target.y*w]) {
        const u = Q.shift();
        done[u.x+u.y*w] = true;

        for (let v of neighbors(u)) {
            if (!done[v.x+v.y*w]) {
                const alt = dist[u.y][u.x] + risk(v);
                if (alt < dist[v.y][v.x]) {
                    dist[v.y][v.x] = alt;
                    pred[v.y][v.x] = u;
                }
            }

			if(!wasinq[v.y][v.x]){		
                insertAt(Q, v, (a,b) => dist[a.y][a.x] < dist[b.y][b.x]);
				wasinq[v.y][v.x] = true;
			}
        }
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

const time = (f) => {
    const s = Date.now();
    const result = f();
    const time = Date.now() - s;
    return { result, time };
}

lines().then((data) => {
    const w = data[0].length, h = data.length;
    const risk = ({x,y}) => data[y]?.[x] ? +data[y][x] : 10;
    const part1 = time(() => dijkstra(data, {x:0,y:0}, {x:w-1,y:h-1}, risk).map((pos) => risk(pos)).reduce((acc,val) => acc+val, 0) - risk({x:0,y:0}));
    console.log('part 1:', part1.result, ` (elapsed time ${part1.time} ms)`);
    
    const data2 = fullmap(data, w, h);
    const risk2 = ({x,y}) => data2[y]?.[x] ? +data2[y][x] : 10;
    const part2 = time(() => dijkstra(data2, {x:0,y:0}, {x:5*w-1,y:5*h-1}, risk2).map((pos) => risk2(pos)).reduce((acc,val) => acc+val, 0) - risk({x:0,y:0}));
    console.log('part 2:', part2.result, ` (elapsed time ${part2.time} ms)`);
});
