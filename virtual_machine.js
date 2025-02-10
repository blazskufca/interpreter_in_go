/*
Virtual machine is a computer built with software.
A virtual machine can be anything. A function, a struct, an object, a module, or even a whole program.

A virtual machine has a run loop that goes through the fetch-decode-execute cycle, just like a computer.

It has a program counter; it fetches instructions; it decodes and executes them. It also has a stack, just like a real
computer.
Sometimes it has a call stack and sometimes even registers. All built in software.

There are two general types of virtual machines:
    - Stack based virtual machines, like the one below:
        - Simpler to build, but the tradeoff is that it requires lengthy programs for even the simplest of things
        - Take a look at the program array at the bottom, it has 8 elements just to perform: (3 + 4) - 5
        - This is a problem if you're trying to optimize it

    - Register virtual machine - Has (virtual i.e. software defined) registers and performs assigned operations with them.
        - Despite being a register virtual machine, it still has a stack, however this stack is mostly reserved for things
        - like a call stacks, but it's not being used for every single instruction in the program.
        -  It's more performant but also more complex to build

Why use a virtual machine at all?
    - Most of the time you would want the language you're using/building to be universal. It should be able to execute
        all the possible programs, i.e. not just the function you've/the language designer built into the language itself.
        Mimicking real computers is that fastest possible way to achieve this.

    - But if this is the goal, why not skip the whole software defined computers, i.e. virtual machines, and just compile
        to actual machine code? It would be even faster...
        - Portability! - A virtual machine runs on all platform on which the host language can run. If you use something
        like Go to implement your virtual machines, that's a lot of supported platforms with basically 0 additional work.
        This is not the case if you're compiling directly to machine code (at lest not without quite a bit of additional work)!

     - Virtual machines are domain specific - You might not need your virtual machine to be completely universal like real machines.
        - That means decreased complexity and thought this fact, possibly increased performance (since again, you might not need
            complete universality - You can possibly make tradeoffs which real, universal, machines are unable to make!

Virtual machines execute bytecode. This bytecode tells it to pop, push, add, etc...
It's called a bytecode because instructions in each opcode are one byte (as in size).
Since bytecode (and real machine code) is binary, names like "pop", "push", "add", "mov" are mnemonics! They are meant for
you, the person looking at the bytecode/machine code, so you don't have to remember which sequence of bits is supposed to do what
from memory.

The operands to these opcodes are also binary and are placed next to the opcodes. Or maybe they are not. It depends
on weather the opcode need an operand/parameter or not. They don't have to be 1 byte long either. They can be, but it's
not a requirement. Think about the following instruction:

                                                        push 300

The "300" integer operand here could be encoded as 2 bytes since the number it's trying to encode is larger than 255.

If an operand consist of multiple bytes, it can be encoded in two ways:
    - Least significant byte (LSB) comes first and it's stored in the lower memory address. This is known as Little-Endian.
    - Big endian is the opposite of little endian. Most significant byte (MSB) comes first. Big endian is also known as
        Network order since it's very prominent in networking protocols.

To recap, bytecode is a domain-specific language for a domain-specific machine.
It’s the bespoke machine language for the custom-built virtual machine.
*/

let virtualMachine = function (program) {
    let programCounter = 0;
    let stack = [];
    let stackPointer = 0;

    while (programCounter < program.length) {
        let currentInstruction = program[programCounter];

        switch (currentInstruction) {
            case "PUSH": // Adds new instruction onto the stack
                stack[stackPointer] = program[programCounter + 1];
                stackPointer++;
                programCounter++;
                break;
            case "ADD": // Does the plus arithmetic operation on previous instruction on the stack
                let rightAdd = stack[stackPointer - 1];
                stackPointer--;
                let leftAdd = stack[stackPointer - 1];
                stackPointer--;
                stack[stackPointer] = leftAdd + rightAdd;
                stackPointer++;
                break;
            case "MINUS": // Does the minus arithmetic operation on previous instruction on the stack
                let rightMinus = stack[stackPointer - 1];
                stackPointer--;
                let leftMinus = stack[stackPointer - 1];
                stackPointer--;
                stack[stackPointer] = leftMinus - rightMinus;
                stackPointer++;
                break;
        }
        programCounter++;
    }
    console.log("stack top:", stack[stackPointer - 1]);
}

// Expression encoded in these instructions is: (3 + 4) - 5
let program = [
    "PUSH", 3,
    "PUSH", 4,
    "ADD",
    "PUSH", 5,
    "MINUS"
];

virtualMachine(program)
