
import { lines } from './input.mjs';

(async () => {
    const adjacent_pairs = (arr) => {
        const arr2 = arr.slice(1);
        return arr2.map((_, i) => [arr[i], arr2[i]]);
    };

    const data = (await lines()).map((n) => +n);

    const num_larger = (data) => adjacent_pairs(data).filter(([a, b]) => b > a).length;

    console.log('part 1: ', num_larger(data));

    const sum = (arr) => arr.reduce((acc, val) => acc+val);
    const windowsums = (n, data) => 
        Array.from({ length: data.length - n + 1},
            (_, i) => sum(data.slice(i, i+n)));
    
    console.log('part 2: ', num_larger(windowsums(3, data)));
})();


