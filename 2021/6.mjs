
import { lines } from './input.mjs';

(async () => {
    const sum = (data) => data.reduce((sum, val) => val + sum, 0);
    const count_if = (data, pred) => data.reduce((sum, val) => pred(val) ? sum+1 : sum, 0);

    const data = eval('['+(await lines())[0]+']');
    const v = Array.from({length: 9}, (_, i) => count_if(data, (val) => val === i));
    const calc = (v, cycle) => {
        if (cycle === 0) return sum(v);
        const z = v[0];
        v.forEach((_, i) => { v[i] = v[i+1]; });
        v[6] += v[8] = z;
        return calc(v, cycle-1);
    };

    console.log('part 1 : ', calc([...v], 80));
    console.log('part 2 : ', calc([...v], 256));    
})();
