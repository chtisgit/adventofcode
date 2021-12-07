
import { lines } from './input.mjs';

function* boundingrange(data) {
    for(let begin = Math.min(...data), end = Math.max(...data); begin <= end; begin++)
        yield begin;
}

(async () => {
    const unique = (data) => data.filter((val, i) => val !== data[i-1]);
    const euler = (n) => n*(n+1)/2;

    const data = eval('['+(await lines())[0]+']');
    data.sort();
    const fuellist1 = [...boundingrange(data)].map((pos) => data.reduce((acc, val) => acc + Math.abs(pos - val), 0));
    const res1 = Math.min(...fuellist1);
    console.log(`part 1 : ${res1}`);

    const fuellist2 = [...boundingrange(data)].map((pos) => data.reduce((acc, val) => acc + euler(Math.abs(pos - val)), 0));
    const res2 = Math.min(...fuellist2);
    console.log(`part 2 : ${res2}`);
})();
