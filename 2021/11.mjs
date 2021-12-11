
import { lines } from './input.mjs';

(async () => {
    class Octopus {
        constructor(energy,x,y) { this.energy = energy; this.x = x; this.y = y; }
        inc() { this.energy++; }
        isFlash() { return this.energy > 9 && !this.hasFlashed; }
        flash() { this.hasFlashed = true; }
        reset() { if (this.hasFlashed) this.energy = 0; this.hasFlashed = false; }
    }

    const data = (await lines());

    const octopuses = {};
    data.forEach((line,y) => [...line].forEach((c,x) => octopuses[`${x}~${y}`] = new Octopus(+c,x,y)));
    
    const neighbors = ({x,y}) => [{x:x-1,y:y-1},{x:x,y:y-1},{x:x+1,y:y-1},{x:x-1,y:y},{x:x+1,y:y},{x:x-1,y:y+1},{x:x,y:y+1},{x:x+1,y:y+1}];
    const compareCoords = ({x:x1,y:y1}, {x:x2,y:y2}) => y1 === y2 ? x1-x2 : y1-y2;
    const uniqueCoords = (val, i, arr) => i === 0 || compareCoords(val, arr[i-1]) !== 0;
    
    let flashes = 0, part1 = null, part2 = null;
    const all = Object.values(octopuses);
    for (let step = 0; part1 === null || part2 === null; step++) {
        let inclist = all;
        console.log(`step ${step+1}`);
        while (inclist.length !== 0) {
            inclist.forEach((oct) => oct.inc());
            const flashers = inclist.filter((oct) => oct.isFlash()).filter(uniqueCoords);
            inclist = flashers
                .map(neighbors)
                .flat(1)
                .sort(compareCoords)
                .map(({x,y}) => octopuses[`${x}~${y}`])
                .filter((oct) => oct !== undefined);
            flashers.forEach((oct) => oct.flash());
            flashes += flashers.length;
        }
        all.forEach((oct) => oct.reset());

        if (step === 99)
            part1 = flashes;
        if (all.reduce((acc, oct) => oct.energy === 0 ? acc+1 : acc, 0) === all.length)
            part2 = step+1;
    }

    console.log('part 1: ',part1);
    console.log('part 2: ',part2);

})();
