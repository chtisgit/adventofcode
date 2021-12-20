import { lines } from './input.mjs';

const border = (data, bg) => {
    const len = data[0].length+2, dots = bg.repeat(len);
    return [dots].concat(...data.map((line) => bg+line+bg), [dots]);
}

const enhance = (data, enh, bg='.') => {
    data = border(data, bg);
    const binary = {'.': '0', '#': '1' };
    binary[undefined] = binary[bg];
    const neighbors = ({x,y}) => [{x:x-1,y:y-1},{x:x,y:y-1},{x:x+1,y:y-1},{x:x-1,y:y},{x,y},{x:x+1,y:y},{x:x-1,y:y+1},{x:x,y:y+1},{x:x+1,y:y+1}];
    return data.map((line, y) => [...line].map((_, x) => enh[parseInt(neighbors({x,y}).map(({x,y}) => binary[data?.[y]?.[x]]).join(''), 2)]).join(''));
}

lines().then((data) => {
    const enh = data[0];
    data.splice(0, 2);

    let lit = [null], bg = '.';
    for (let i = 1; i <= 50; i++) {
        data = enhance(data, enh, bg);
        //console.log(data.join('\n'));
        lit.push(data.reduce((acc, line) => acc + [...line].reduce((acc, ch) => ch === '#' ? acc+1 : acc, 0), 0));
        bg = bg === '.' ? enh[0] : enh[enh.length-1];
    }

    console.log('part 1: ', lit[2]);
    console.log('part 2: ', lit[50]);
});