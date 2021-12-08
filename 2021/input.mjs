import * as readline from 'readline';
import {stdin, stdout} from 'process';

export function input() {
    return readline.createInterface({
        input: stdin,
        output: stdout,
    });
}

export function lines() {    
    return new Promise((resolve, reject) => {
        const rl = input();

        let lines = [];

        rl.on('line', (line) => {
            lines.push(line);
        });

        rl.on('SIGINT', () => {
            reject('SIGINT');
        });

        rl.on('close', () => {
            resolve(lines);
        });
        
    });
}