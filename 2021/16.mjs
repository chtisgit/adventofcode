
import { lines } from './input.mjs';

const time = (f) => {
    const s = Date.now();
    const result = f();
    const time = Date.now() - s;
    return { result, time };
}

const extend = (b,len) => b.length < len ? extend('0'+b,len) : b;
const hex2bin = (x,len) => extend(parseInt(x, 16).toString(2), len ?? 0);

class DecodeErr extends Error {
    constructor(s, bit) { super(s); this.bit = bit; }
}

function* bitreader(bin) {
    for (let bit of bin) yield bit;
}

const decode = (rd, bits, acc = '') => {
    const i = rd.next();
    if (i.value === undefined && i.done) throw new DecodeErr('unexpected EOF', bits);    
    return bits === 1 ? {value: parseInt(acc+i.value,2), bits: acc.length+1, done: i.done} : decode(rd, bits-1, acc+i.value)
};

const decodeMessage = (rd) => {
    const parts = [];
    try {
        parts.push(decode(rd,3));
    } catch(err) {
        if (err instanceof DecodeErr && err.bit === 3)
            return {bits:0, done: true};
        throw err;
    }

    parts.push(decode(rd,3));

    if (parts[1].value === 4) { // literal value
        do {
            const val = decode(rd,5);
            parts.push(val);
        }while(parts[parts.length-1].value >= 16);
    } else { // operator
        const lengthType = decode(rd,1);
        parts.push(lengthType);

        if (lengthType.value === 0) {
            const bits = decode(rd, 15);
            parts.push(bits);
            
            for (let i = 0; i < bits.value;) {
                const m = decodeMessage(rd, false)
                parts.push(m);
                i += m.bits;
            }
        } else {
            const num = decode(rd, 11);
            parts.push(num);
            for (let i = num.value; i !== 0; i--)
                parts.push(decodeMessage(rd, false));
        }
    }

    const bitsread = parts.reduce((acc,{bits}) => acc+bits, 0);
    return {parts, done: false, bits: bitsread};
};

function *logger(rd) {
    for (;;) {
        const z = rd.next();
        if (z.done && z.value === undefined) break;
        console.log('yield ', z);
        yield z.value;
    }
}

const literal = (msg) => parseInt(msg.parts.slice(2).reduce((bin, {value}) => bin + extend((value & 0xF).toString(2), 4), ''),2)


const addVersionNums = (msg) => {
    let sum = msg.parts[0].value;
    for (const part of msg.parts.filter((part) => part.parts)) {
        sum += addVersionNums(part);
    }
    return sum;
}

const toJS = (msg, level = 0) => {
    if (!msg) return null;
    let prefix = '';
    if (level === 0) {
        prefix = 'const sum = (list) => list.reduce((acc,val) => acc+val, 0);'
        prefix += 'const product = (list) => list.reduce((acc,val) => acc*val, 1);'
    }

    const type = msg.parts[1].value;
    if (type === 4) {
        return prefix+literal(msg);
    }

    const commatated = msg.parts?.slice(4).map((msg) => toJS(msg, level+1)).join(',');
    const a = toJS(msg.parts?.[4], level+1), b = toJS(msg.parts?.[5], level+1);
    switch (type) {
    case 0: // sum
        return `${prefix} sum([${commatated}])`;
    case 1: // product
        return `${prefix} product([${commatated}])`;
    case 2: // min
        return `${prefix} Math.min(${commatated})`;
    case 3: // max
        return `${prefix} Math.max(${commatated})`;
    case 5: // greater than
        return `${prefix} (${a} > ${b} ? 1 : 0)`
    case 6: // less than
        return `${prefix} (${a} < ${b} ? 1 : 0)`;
    case 7: // equal to
        return `${prefix} (${a} === ${b} ? 1 : 0)`;
    }
};

lines().then((data) => {
    const bin = [...data[0]].map((ch) => hex2bin(ch,4)).join('');
    const rd = bitreader(bin);

    try {
        const msg = decodeMessage((rd));
        console.log('part 1: ', addVersionNums(msg));
        const code = toJS(msg);
        console.log('part 2: ', eval(code));
        console.log('code: ', code);
    } catch(s) {
        console.log('error: ',s.stack);
    }

});
