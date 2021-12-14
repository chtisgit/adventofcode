
import { lines } from './input.mjs';

function* twos(str) { for (let i = 1; i !== str.length; i++) yield str.substr(i-1,2); }

lines().then((data) => {
    const inc = (m, key, step = 1) => (m[key] = (m[key]??0) + step, m);
    const counts = [...data[0]].reduce((m, ch) => inc(m,ch), {});
    const m = [...twos(data[0])].reduce((m, str) => inc(m, str), {});
    const rep = data.slice(2)
        .map((line) => line.split(' -> '))
        .reduce((m, [str,res]) => (m[str] = [str[0]+res,res+str[1],res], m), {});

    const qtys = [];
    for (let i = 0; i !== 41; i++) {
        qtys.push(Math.max(...Object.values(counts)) - Math.min(...Object.values(counts)));

        const changes = [];
        Object.entries(m).forEach(([str,cnt]) => {
            const res = rep[str];
            if (res && cnt) {
                changes.push([str, -cnt], [res[0], cnt], [res[1], cnt]);
                inc(counts, res[2], cnt);
            }
        });

        changes.forEach(([str,cnt]) => inc(m, str, cnt));
    }

    console.log('part 1: ', qtys[10]);
    console.log('part 2: ', qtys[40]);
});
