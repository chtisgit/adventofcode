
import { lines } from './input.mjs';

(async () => {
    class Bitcounter {
        constructor(pos) { this.m = {}; this.pos = pos; }
        process(s) {
            let v = s[this.pos];
            if (this.m[v]) this.m[v]++; else this.m[v] = 1;
        }
        commonest(v) {
            return Object.entries(this.m)
                .reduce((acc, [char, num]) => 
                    acc[1] < num ? [char, num] : acc, ['', -1])[0];
        }
        tie() { return this.m['0'] === this.m['1']; }
    }

    class Evaluator {
        constructor(bits) { this.ctrs = Array.from({length: data[0].length}, (_, i) => new Bitcounter(i)); }
        process(s) { this.ctrs.forEach((ctr) => ctr.process(s)); }
        gamma() { return parseInt(this.ctrs.map((ctr) => ctr.commonest()).join(''), 2); }
        epsilon() { return parseInt(this.ctrs.map((ctr) => ctr.commonest() == '1' ? '0' : '1').join(''), 2); }
        first() { return this.ctrs[0].tie() ? '1' : this.ctrs[0].commonest(); }
    }

    const data = (await lines()).filter((line) => line !== '');
    const e = new Evaluator(data[0].length);

    data.forEach((s) => e.process(s));

    console.log('gamma: ', e.gamma());
    console.log('epsilon: ', e.epsilon());
    console.log('part 1: ', e.gamma() * e.epsilon());

    const part2Index = (data, bitf) => {
        if (data.length === 1)
            return data[0].index;
        
        const e = new Evaluator(data[0].s.length);
        data.forEach(({s}) => e.process(s));
        const prefix = bitf(e.first());
        return part2Index(data.filter(({s}) => s.startsWith(prefix)).map(({s, index}) => ({s : s.substr(1), index: index})), bitf);
    };

    const o2  = data[part2Index(data.map((s,i) => ({s:s, index: i})), (bit) => bit)];
    const co2 = data[part2Index(data.map((s,i) => ({s:s, index: i})), (bit) => bit === '1' ? '0' : '1')]

    console.log('o2:  ', parseInt(o2, 2));
    console.log('co2: ', parseInt(co2, 2));
    console.log('part 2: ', parseInt(o2, 2)*parseInt(co2, 2));


})();


