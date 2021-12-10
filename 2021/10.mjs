
import { lines } from './input.mjs';

(async () => {
    const data = (await lines());
    const scores1 = { ')': 3, ']': 57, '}': 1197, '>': 25137 };
    const scores2 = { ')': 1, ']': 2, '}': 3, '>': 4 };
    const invert = { '(': ')', '[': ']', '{': '}', '<': '>' };

    const remove_pairs = (line) => {
        while ( /(\(\)|\[\]|<>|\{\})/g.test(line))
            line = line.replaceAll( /(\(\)|\[\]|<>|\{\})/g, '');
        return line;
    };

    const points1 = data
        .map((line) => /[)\]}>]/g.exec(remove_pairs(line)))
        .filter((res) => res !== null)
        .map((res) => scores1[res[0]])
        .reduce((acc,val) => acc+val, 0);

    const points2list = data
        .map((line) => {
            const begin = remove_pairs(line);
            return /[)\]}>]/g.exec(begin) === null ? begin : null;
        })
        .filter((line) => line !== null)
        .map((line) => [...line].map((c) => invert[c]))
        .map((symbols) => symbols.reduceRight((acc,c) => acc*5+scores2[c], 0))
        .sort((a,b) => a-b);
    const points2 = points2list[parseInt(points2list.length/2)];

    console.log(`part 1: ${points1}`);
    console.log(`part 2: ${points2}`);
    
})();
