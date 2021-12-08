import { Subject } from 'rxjs';
import { input } from './input.mjs';

async function forwardInput(sj) {
    for await(const line of input()) {
        sj.next(line);
    }
    sj.complete();
}

export function inputSubject(){
    const sj = new Subject();
    forwardInput(sj);
    return sj;
}
