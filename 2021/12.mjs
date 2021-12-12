
import { lines } from './input.mjs';

lines().then((data) => {
    const edges = [];

    data.forEach((line) => {
        const [n1,n2] = line.split('-');
        if (n2 !== 'start') edges.push([n1,n2]);
        if (n1 !== 'start') edges.push([n2,n1]);
    });
    
    const findPaths = (prefix, visited, part2, cb) => {
        const current = prefix[prefix.length-1];
        if (current === 'end') {
            cb(prefix);
            return;
        }

        visited[current] = (visited[current] ?? 0) + 1;
        const possibilities = edges.map(([a,b]) => a === current ? b : null).filter((next) => next !== null);
        for (const next of possibilities) {
            if (next[0] === next[0].toUpperCase() || (visited[next] ?? 0) < 1
                || (part2 && (visited[next] ?? 0) < 2 && !Object.entries(visited).find(([name,cnt]) => 
                    name[0] === name[0].toLowerCase() && cnt === 2
                ))) {
                prefix.push(next);
                findPaths(prefix, visited, part2, cb);
                prefix.pop();
            }
        }

        visited[current]--;
    };

    let part1 = 0, part2 = 0;
    findPaths(['start'], {}, false, (path) => part1++);
    findPaths(['start'], {}, true, (path) => part2++);

    console.log('part 1: ', part1);
    console.log('part 2: ', part2);
});
