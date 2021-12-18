import { lines } from './input.mjs';

const reduce = (acc) => {
    let depth = 0;
    for (let i = 0; i !== acc.length; i++) {
        if (acc[i] === '[') depth++;
        if (acc[i] === ']') depth--;
            
        if (depth > 4) {
            const match = /^\[([0-9]+),([0-9]+)\]/.exec(acc.slice(i));
            if (match) {
                acc = acc.slice(0,i) + '0' + acc.slice(i+match[0].length);
//                console.log('explode ', match[0]);
                const left = acc.slice(0, i-1).match(/[0-9]+/g)?.pop();
                const right = acc.slice(i+1).match(/[0-9]+/g)?.[0];
                
                if (right) {
                    const j = acc.indexOf(right, i+1);
                    acc = acc.slice(0, j) + ((+match[2])+(+right)) + acc.slice(j+right.length); 
                }
                
                if (left) {
                    const j = acc.slice(0, i-1).lastIndexOf(left);
                    acc = acc.slice(0, j) + ((+match[1])+(+left)) + acc.slice(j+left.length);
                }
                
                return [acc, true];
            }      
        }
    }

    for (let i = 0; i !== acc.length; i++) {
        const num = /^[0-9]+/.exec(acc.slice(i))?.[0];   
        if (num !== undefined && +num >= 10) {
//            console.log('split ', num);
            acc = acc.slice(0,i) + `[${Math.floor((+num)/2)},${Math.ceil((+num)/2)}]` + acc.slice(i+num.length);
            return [acc, true];
        }
    }

    return [acc, false];
}

const add = (a,b) => {
    let acc = `[${a},${b}]`, reduced = true;
    while (reduced) { [acc, reduced] = reduce(acc); }
    return acc;
}

const magn = (arr) => typeof arr === 'number' ? arr : 3*magn(arr[0])+2*magn(arr[1]);

lines().then((data) => {
    const part1 = magn(JSON.parse(data.reduce((acc,line) => acc === null ? line : add(acc, line), null)));
    console.log('part 1: ', part1);

    const part2 = Array.from({length: data.length * data.length}, (_, i) => [i / data.length >>> 0, i % data.length])
        .map(([i,j]) => add(data[i],data[j]))
        .map((res) => magn(JSON.parse(res)))
        .reduce((max, val) => val > max ? val : max, 0);
    console.log('part 2: ', part2);
});