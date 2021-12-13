
import { lines } from './input.mjs';

lines().then((data) => {
    let dots = [], instrs = [];

    const split = data.indexOf('');
    data.slice(0,split).forEach((line) => {
        const [x,y] = line.split(',').map((n) => +n);
        dots.push({x,y});
    });
    data.slice(split+1).forEach((line) => {
        const [_, axis,num] = /([xy])=([0-9]+)/.exec(line);
        instrs.push([axis, +num]);
    })

    const compareCoords = ({x:x1,y:y1}, {x:x2,y:y2}) => y1 === y2 ? x1-x2 : y1-y2;
    const uniqueCoords = (val, i, arr) => i === 0 || compareCoords(val, arr[i-1]) !== 0;

    let part1 = null;
    instrs.forEach(([coord, num], step) => {
        dots.forEach((p) => (p[coord] > num) ? p[coord] = num - (p[coord] - num) : null);
        dots.sort(compareCoords)
        dots = dots.filter(uniqueCoords);

        if (step === 0) part1 = dots.length;
    });

    const print = (dots, ly = 0) => {
        const thisLine = dots.filter(({y}) => y === ly );
        if (thisLine.length === 0) return;
        
        console.log(Array.from({length: Math.max(...thisLine.map(({x}) => x))+1}, (_, i) => 
                thisLine.find(({x}) => x === i) ? '#' : ' ').join(''));
        
        print(dots.filter(({y}) => y > ly), ly+1);
    };

    console.log(`part 1: ${part1}`);
    console.log(`part 2:`);
    print(dots);    
});
