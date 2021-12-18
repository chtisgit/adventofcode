
import { lines } from './input.mjs';

const reduce = (acc) => {
    let depth = 0;
    for (let i = 0; i !== acc.length; i++) {
        let ch = acc[i];
        let num = null;
    
        if (ch === '[') depth++;
        if (ch === ']') depth--;
            
        if (depth > 4) {
            const match = /^\[([0-9]+),([0-9]+)\]/.exec(acc.substr(i));
            if (match) {
                acc = acc.substr(0,i) + '0' + acc.substr(i+match[0].length);
//                console.log('explode ', match[0]);
                
                let left = acc.substr(0, i-1).match(/[0-9]+/g);
                left = left?.[left?.length-1];
                const right = acc.substr(i+1).match(/[0-9]+/g)?.[0];
                
                if (right) {
                    const j = acc.indexOf(right, i+1);
                    acc = acc.substr(0, j) + ((+match[2])+(+right)) + acc.substr(j+right.length); 
                }
                
                if (left) {
                    const j = acc.substr(0, i-1).lastIndexOf(left);
                    acc = acc.substr(0, j) + ((+match[1])+(+left)) + acc.substr(j+left.length);
                }
                
                return acc;
            }      
        }
    }

    for (let i = 0; i !== acc.length; i++) {
        let ch = acc[i];
        let num = null;
        if (/[0-9]/.test(ch)) num = /^[0-9]+/.exec(acc.substr(i))[0];
            
        if (num !== null && +num >= 10) {
//            console.log('split ', num);
            acc = acc.substr(0,i) + `[${Math.floor((+num)/2)},${Math.ceil((+num)/2)}]` + acc.substr(i+num.length);
            return acc;
        }
    }

    return null;
}

const add = (a,b) => {
    let acc = `[${a},${b}]`;
    do {
        a = acc;
        acc = reduce(acc);
    } while(acc !== null);
    
    return a;
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