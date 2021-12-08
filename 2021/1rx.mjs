import { filter, map, pairwise, count, bufferCount } from 'rxjs';
import { inputSubject } from './rxinput.mjs';

const stdin = inputSubject();

stdin
    .pipe(
        map((v) => +v),
        pairwise(),
        filter(([a,b]) => b > a),
        count(),
    )
    .subscribe((cnt) => {
        console.log('part 1: ', cnt);
    });

stdin
    .pipe(
        map((v) => +v),
        pairwise(),
        pairwise(),
        map((arr) => arr.flat()),
        map(([a,b,c,d]) => a+b+d),
        pairwise(),
        filter(([a,b]) => b > a),
        count(),
    )
    .subscribe((cnt) => {
        console.log('part 2: ', cnt);
    });
