
import { lines } from './input.mjs';

(async () => {
    const m = [
        { cmd: 'down', code: 'y += '},
        { cmd: 'up', code: 'y -= '},
        { cmd: 'forward', code: 'x += '},
    ];

    const commands = await lines();

    let x = 0, y = 0;
    eval(commands.map((line) => {
        m.forEach(({cmd, code}) => { line = line.replaceAll(cmd, code); });
        return line;
    }).join(';'));

    console.log(`x: ${x}  y: ${y}`);
    console.log(`part 1: ${x*y}`);

    let aim = 0;
    const m2 = [
        { cmd: 'down', code: 'aim += '},
        { cmd: 'up', code: 'aim -= '},
        { cmd: /forward ([0-9]+)/g, code: (_, num) => `x += ${num}; y += ${num} * aim` },
    ];
    
    x = 0;
    y = 0;
    eval(commands.map((line) => {
        m2.forEach(({cmd, code}) => { line = line.replaceAll(cmd, code); });
        return line;
    }).join(';'));

    console.log(`x: ${x}  y: ${y}`);
    console.log(`part 2: ${x*y}`);

})();


