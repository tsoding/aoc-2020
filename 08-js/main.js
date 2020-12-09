const fs = require("fs");
const assert = require("assert");

function execute(program) {
    let accumulator = 0;
    let ip = 0;
    let visited = [];

    while (ip < program.length && !visited.includes(ip)) {
        visited.push(ip);
        switch (program[ip].instr) {
        case "nop":
            ip += 1;
            break;
        case "acc":
            accumulator += program[ip].arg;
            ip += 1;
            break;
        case "jmp":
            ip += program[ip].arg;
            break;
        default:
            assert(false, `unknown instructions ${program[ip].instr}`);
        }
    }

    return {
        accumulator: accumulator,
        visited: visited,
        terminated: ip >= program.length,
    };
}

function part1(program) {
    return execute(program).accumulator;
}

function flip(instr) {
    switch(instr) {
    case "jmp": return "nop";
    case "nop": return "jmp";
    default: assert(false, `unexpected instruction ${instr}`);
    }
}

function part2(program) {
    let visited = execute(program).visited;

    for (let ip of visited) {
        if (program[ip].instr != "acc") {
            program[ip].instr = flip(program[ip].instr);
            let result = execute(program);
            if (result.terminated) {
                return result.accumulator;
            }
            program[ip].instr = flip(program[ip].instr);
        }
    }

    assert(false, "Could not find terminating solution");
}

function solveFile(filePath) {
    let program = fs.readFileSync(filePath)
        .toString()
        .trim()
        .split('\n')
        .map(x => {
            let [instr, arg] = x.split(' ');
            return {
                instr: instr,
                arg: parseInt(arg)
            };
        });

    console.log(`Input file ${filePath}`);
    console.log(`Part 1: ${part1(program)}`);
    console.log(`Part 2: ${part2(program)}`);
}

for (let filePath of process.argv.slice(2)) {
    solveFile(filePath);
}
