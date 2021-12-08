
import { lines } from './input.mjs';

(async () => {
    const data = (await lines()).map((line) => line.split(' | ').map((str) => str.split(' ').map((code) => [...code].sort().join(''))));
    
    // checks if code1 contains code2
    const contains = (code1,code2) => [...code2].every((ch) => code1.indexOf(ch) !== -1);
    
    // removes characters that are in code2 from code1
    const sub = (code1,code2) => [...code1].filter((ch) => code2.indexOf(ch) === -1).join('');
    
    const sherlock = (clues) => {
        clues['9'] = clues['9'].filter((code) => contains(code, clues['4'][0]));
        clues['0'] = clues['0'].filter((code) => code !== clues['9'][0]);
        clues['6'] = clues['6'].filter((code) => code !== clues['9'][0]);

        clues['0'] = clues['0'].filter((code) => contains(code, clues['1'][0]));
        clues['6'] = clues['6'].filter((code) => code !== clues['0'][0]);

        clues['3'] = clues['3'].filter((code) => contains(code, clues['7'][0]));
        clues['2'] = clues['2'].filter((code) => code !== clues['3'][0]);
        clues['5'] = clues['5'].filter((code) => code !== clues['3'][0]);
    
        clues['5'] = clues['5'].filter((code) => contains(code, sub(clues['4'][0], clues['1'][0])));
        clues['2'] = clues['2'].filter((code) => code !== clues['5'][0]);
    };
    const invert = (clues) => Object.fromEntries(Object.entries(clues).map(([key,val]) => [val[0],key]));

    let count1 = 0;
    let outputsum = 0;
    for (const line of data) {
        let clues = {};
        [0,1,2,3,4,5,6,7,8,9].map((v) => ''+v).forEach((v) => clues[v] = []);
        const numm = {
            '2': [1], '3': [7], '4': [4],
            '5': [2,3,5], '6': [0,6,9], '7': [8],
        };

        count1 += line[1].filter((code) => numm[''+code.length].length === 1).length;

        for (const out of line[0].concat(line[1])) {
            numm[''+out.length].forEach((z) => clues[''+z].push(out));
        }
        sherlock(clues);
        const solver = invert(clues);
        
        outputsum += +line[1].map((code) => solver[code]).join('');
    }

    console.log(`part 1: ${count1}`);
    console.log(`part 2: ${outputsum}`);
})();
