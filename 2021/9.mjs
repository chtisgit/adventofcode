
import { lines } from './input.mjs';

(async () => {
    const data = (await lines());

    let risk = 0;
    const lps = [];
    for (let y = 0; y !== data.length; y++) {
        for (let x = 0; x !== data[y].length; x++) {
            const n = +data[y][x];
            if (!(n >= +data[y-1]?.[x]) &&
                !(n >= +data[y+1]?.[x]) &&
                !(n >= +data[y][x-1]) &&
                !(n >= +data[y][x+1])) {
                    lps.push({x,y, height: n});
                    risk += 1 + n;
            }
        }
    }

    const basinsize = ({x,y}, visited) => {
        const key = `${x}~${y}`;
        if (visited[key]) return 0;
        if (!data[y]?.[x]) return 0;
        const n = +data[y][x];
        if (n === 9) return 0;
        visited[key] = true;
        return basinsize({x:x-1,y}, visited) + basinsize({x:x+1,y}, visited) +
            basinsize({x,y:y-1}, visited) + basinsize({x,y:y+1}, visited) + 1;
    }

    const largest3 = lps.map((p) => basinsize(p, {})).sort((a,b) => b-a).slice(0,3);
    
    console.log(`part 1: ${risk}`);
    console.log(`part 2: ${largest3[0]*largest3[1]*largest3[2]}`);
})();
