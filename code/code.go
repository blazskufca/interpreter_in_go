package code

import (
	"bytes"
	"encoding/binary"
	"fmt"
)

/*
Expressions, like "1+2" travel through the following modules in bytecode interpreter:

											+--------+     +--------+     +--------+     +---------------+
											| Lexer  | --> | Parser | --> |Compiler| --> |Virtual Machine|
											+--------+     +--------+     +--------+     +---------------+

During this process monkey source code itself is transformed into the following data structures:

											+--------+     +---------+     +-----+     +----------+     +---------+
											| String | --> | Tokens  | --> | AST | --> | Bytecode | --> | Objects |
											+--------+     +---------+     +-----+     +----------+     +---------+

Everything upto and including AST is also a part of the tree-walking interpreter, where they differ are the data
structures after the AST.

We'll be building a stack based virtual machine for the bytecode interpreter, because the goal is learning not necessarily raw performance.

Direct effect of this is that, since everything lives on a stack, we have to do stack arithmetic. General idea behind
this is already described in ./../virtual_machine.js, but the basic idea is the following. If we want to calculate 1+2,
we first push 1 onto the stack, then push 2 onto the stack and then finally push the add instruction.
Three, the result of 1 + 2, replaces both operands on the stack and is now on top of the stack:

												Top --> +----------+    +----------+
														|    2     |    |    3     |
														+----------+    +----------+
														|    1     |    |          |
														+----------+    +----------+
														   BEFORE           AFTER
*/
/*
															COMPILING CONDITIONALS (if statements, etc..):

In our evaluator/in-place evaluator evaluating 'if/else' statements was pretty simple one we've understood what we had to do, and that was:
	1. Evaluate the condition (i.e. if (<condition>)

	2. If the condition is truthy, evaluate the consequence (i.e. if (<truthy>) { <consequence> })

	3. If the condition did not evaluate to truthy value, evaluate the 'alternative' (i.e. if (<false>) { <skip consequence> } else { <alternative, if present> })

	4. If there was no alternative branch and the condition evaluated to false, return null instead of evaluating alternative

This was only possible because we had our AST at hand (both node, or rather all three - condition node, consequence node and optional alternative node)
In bytecode however, we don't have them at hand, because we flatten the AST...We just have a linear sequence of bytes - The instructions.

So how do we evaluate conditionals then?
Just doing it in linear fashion would result in 100% wrong result since all the branches would be evaluated in every case...

The answer is in the same way a real machine does it...By moving around the instruction pointer!
And you move the instruction pointer with a "jmp" instruction, and it's conditional variants
(in x86_64 assembly for example, there is 'je' - Jump if condition is equal, etc...):

													+------------------+
													|   OpConstant 0   |
													+------------------+
													|   OpConstant 1   |
													+------------------+
													|  OpGreaterThan   |
													+------------------+
															|
											+-------+ JUMP_IF_NOT_TRUE
											|				|
											|				v
											|		+------------------+
											|		|   OpConstant 2   |
											|		+------------------+
											|		|   OpConstant 3   |
											|		+------------------+
											|		|      OpAdd       |
											|		+------------------+
											|				|
											|		  JUMP_NO_MATTER_WHAT +-----+
											|				|                   |
											|				|                   |
											|				|                   |
											|				|                   |
											|				|                   |
											|				|                   |
											|				|                   |
											|				|                   |
											|				|                   |
											|				|                   |
											|				v                   |
											|		+------------------+        |
											+-----> |   OpConstant 4   |        |
													+------------------+        |
													|   OpConstant 5   |        |
													+------------------+        |
													|     OpMinus      |        |
													+------------------+        |
															|                   |
														   ...<-----------------+
But how to implement these arrows?

Well why not just use numbers? The jump instruction could take an number, called OFFSET, which can be either absolute or relative.

This offset tells the VM where to go when encountering a jump instruction, either in absolute terms, i.e. the number of the next
instruction, or in relative terms, i.e. over how many instructions to jump over.

													+-------------------------+ <---+
											0000    |       OpConstant 0      |		|
													+-------------------------+		|
											0001    |       OpConstant 1      |		| +-----> Condition
													+-------------------------+		|
											0002    |       OpGreaterThan     |		|
													+-------------------------+	<---+
											0003    |  JUMP_IF_NOT_TRUE 0008  |
													+-------------------------+ <---+
											0004    |       OpConstant 2      |		|
													+-------------------------+		|
											0005    |       OpConstant 3      |		| +-----> Consequence
													+-------------------------+		|
											0006    |           OpAdd         |		|
													+-------------------------+ <---+
											0007    | JUMP_NO_MATTER_WHAT 0011|
													+-------------------------+ <---+
											0008    |       OpConstant 4      |		|
													+-------------------------+		|
											0009    |       OpConstant 5      |		| +-----> Alternative
													+-------------------------+		|
											0010    |          OpMinus        |		|
													+-------------------------+	<---+
And that's exactly what we should do!
*/
/*
BINDING VALUES TO NAMES:
We'll be binding values/expressions to identifiers in the following way.

Suppose we have the following monkey code:

														let x = 33;
														let y = 66;
														let z = x + y;

Each name (x, y, z) will get a unique number in bytecode. For simplicity sake we'll just start from 0, and then, we'll do
the following which should be pretty clear from the following diagram:

										+---------------+      +----------------------------------+
										| OpConstant 0  | <--- | Load the "33" onto the stack     |
										+---------------+      +----------------------------------+
										| OpSetGlobal 0 | <--- | Bind value on stack to 0 (x var) |
										+---------------+      +----------------------------------+

										+---------------+      +----------------------------------+
										| OpConstant 1  | <--- | Load the "66" onto the stack     |
										+---------------+      +----------------------------------+
										| OpSetGlobal 1 | <--- | Bind value on stack to 1 (y var) |
										+---------------+      +----------------------------------+

										+---------------+      +------------------------------------+
										| OpGetGlobal 1 | <--- | Push the global bound to 1 (y var) |
										+---------------+      +------------------------------------+
										| OpGetGlobal 0 | <--- | Push the global bound to 0 (x var) |
										+---------------+      +------------------------------------+
										| OpAdd         | <--- | Add them together                  |
										+---------------+      +------------------------------------+
										| OpSetGlobal 2 | <--- | Bind value on stack to 2 (z var)   |
										+---------------+      +------------------------------------+

This is the bytecode/compiler side of things.

In the vm.VM we'll have a "global store" (a slice) and we'll use the OpGetGlobal and OpSetGlobal as indexes into it.

OpSetGlobal will pop the topmost value of the VM stack and save it into the global store at the index encoded in the
instruction.

OpGetGlobal will do the reverse, index into the global store, get the value at the index and load it onto the VM stack...
*/

// Instructions consist of an Opcode (specifies the VM operation) and an arbitrary number of operands (0+).
// Opcode is always 1 byte long, operands can be multibyte.

const (
	OpConstant      Opcode = iota // OpConstant is a ""pointer"" into the compilers constants pool
	OpAdd                         // OpAdd tells the Monkey virtual machine to pop 2 elements from the top of the stack and add them together
	OpPop                         // OpPop instructs the Monkey virtual machine to pop the topmost object of its stack.
	OpSub                         // OpSub is a byte code instruction for arithmetic - operations
	OpMul                         // OpMul is the bytecode instruction for arithmetic * operations
	OpDiv                         // OpDiv is the bytecode instruction for arithmetic / operations
	OpTrue                        // OpTrue is the bytecode instruction for pushing a object.Boolean onto the stack
	OpFalse                       // OpTrue is the bytecode instruction for pushing a object.Boolean onto the stack
	OpEqual                       // OpEqual represents the == in bytecode
	OpNotEqual                    // OpNotEqual represents the != in bytecode
	OpGreaterThan                 // OpGreaterThan represents the > in the bytecode. There is no OpLessThan because instead bytecode is reordered and OpGreaterThan operator is reused.
	OpMinus                       // OpMinus causes the Monkey virtual machine to negate integers. It represents -
	OpBang                        // OpBang causes the Monkey virtual machine to negate booleans. It represents the !
	OpJumpNotTruthy               // OpJumpNotTruthy is the bytecode representation of conditional jump instruction
	OpJump                        // OpJump is the bytecode representation of a non-conditional jump instruction
	OpNull                        // OpNull instruction represents a lack of value in Monkey or rather a *object.Null in Monkey object system
	OpGetGlobal                   // OpGetGlobal will instruct the vm.VM to load the value from global store and load it onto the VM stack
	OpSetGlobal                   // OpSetGlobal will instruct the vm.VM to pop the topmost stack value and store it into the global store at the specified index.
)

type Instructions []byte

// String on type Instructions satisfies the fmt.Stringer interface for debugging/disassembly.
func (ins Instructions) String() string {
	var out bytes.Buffer
	i := 0
	for i < len(ins) {
		def, err := Lookup(ins[i])
		if err != nil {
			_, _ = fmt.Fprintf(&out, "ERROR: %s\n", err)
			continue
		}
		operands, read := ReadOperands(def, ins[i+1:])
		_, _ = fmt.Fprintf(&out, "%04d %s\n", i, ins.fmtInstruction(def, operands))
		i += 1 + read
	}
	return out.String()
}

func (ins Instructions) fmtInstruction(def *Definition, operands []int) string {
	operandCount := len(def.OperandWidths)
	if len(operands) != operandCount {
		return fmt.Sprintf("ERROR: operand len %d does not match defined %d\n",
			len(operands), operandCount)
	}
	switch operandCount {
	case 0:
		return def.Name
	case 1:
		return fmt.Sprintf("%s %d", def.Name, operands[0])
	}
	return fmt.Sprintf("ERROR: unhandled operandCount for %s\n", def.Name)
}

// Opcode is a single byte, which specifies an instruction to perform inside the Monkey virtual machine.
type Opcode byte

// Definition primary use is debuggability. It associates human-readable names and width with an Opcode.
type Definition struct {
	Name          string // Name is human-readable name for this Opcode, i.e. a mnemonic.
	OperandWidths []int  // OperandWidths tells us the expected length of the operands
}

// definitions is a map of all known opcodes.
/*
OpConstant: An index into bytecode's/compiler.Compiler's constant pool. It has a single 2 byte operand, which is the index.

OpPop: Instruct the Monkey vm.VM to pop the topmost element of the stack. It has no operands.

OpNull: Instructs a Monkey vm.VM to put a *object.Null onto its stack. It has no operands.

ARITHMETIC OPERATIONS:

OpAdd: Is the OPCODE for additions on the vm.VM's stack. It has no operands.

OpSub: Instruct the Monkey vm.VM to subtract the last two elements on the stack. It has no operands.

OpMul: Instruct the Monkey vm.VM to multiply the last two elements on the stack. It has no operands.

OpDiv: Instruct the Monkey vm.VM to divide the last two elements on the stack. It has no operands.

BOOLEANS:

OpTrue: Causes the vm.VM to push a object.Boolean with a value of true onto its stack. It has no operands.

OpFalse: Causes the vm.VM to push a object.Boolean with a value of false onto its stack. It has no operands.

OpEqual: Compares the two boolean/truthy objects if they are equal. It has no operands.

OpNotEqual: Compares the two boolean/truthy objects if they are not equal. It has no operands.

OpGreaterThan: Compares the two boolean/truthy objects if left is more than right. It has no operands.

OpMinus: Causes the Monkey vm.VM to negate integers. It has no operands.

OpBang: Causes the Monkey vm.VM to negate booleans. It has no operands.

JUMP INSTRUCTIONS:

OpJumpNotTruthy: Instructs the monkey vm.VM to preform a jump (increment the instruction pointer) if some condition
(two topmost stack elements) is not true (or truthy). It takes one 2 byte operand, which is the address (a number) to
which the address of where the instruction pointer should be moved to.

OpJump: Instructs the monkey vm.VM to preform a jump (increment the instruction pointer). It takes one 2 byte operand,
which is the address (a number) to which the address of where the instruction pointer should be moved to.

NAME BINDING:

OpGetGlobal: Instructs the vm.VM to get the value from the global store and load it onto the stack. It accepts a single
2 byte operand, which is the index of the value in the global store.

OpSetGlobal: Instructs the vm.VM to store the topmost stack value into the global store. It accepts a single 2 byte operand
, which is the index under which the value should be stored in global store.
*/
var definitions = map[Opcode]*Definition{
	OpConstant:      {"OpConstant", []int{2}},
	OpAdd:           {"OpAdd", []int{}},
	OpPop:           {"OpPop", []int{}},
	OpSub:           {"OpSub", []int{}},
	OpMul:           {"OpMul", []int{}},
	OpDiv:           {"OpDiv", []int{}},
	OpTrue:          {"OpTrue", []int{}},
	OpFalse:         {"OpFalse", []int{}},
	OpEqual:         {"OpEqual", []int{}},
	OpNotEqual:      {"OpNotEqual", []int{}},
	OpGreaterThan:   {"OpGreaterThan", []int{}},
	OpMinus:         {"OpMinus", []int{}},
	OpBang:          {"OpBang", []int{}},
	OpJumpNotTruthy: {"OpJumpNotTruthy", []int{2}},
	OpJump:          {"OpJump", []int{2}},
	OpNull:          {"OpNull", []int{}},
	OpGetGlobal:     {"OpGetGlobal", []int{2}},
	OpSetGlobal:     {"OpSetGlobal", []int{2}},
}

// Lookup looks up the byte in the definitions map.
// If the op code/byte is not known an error is returned to the caller.
// Otherwise, a *Definition is returned to the caller.
func Lookup(op byte) (*Definition, error) {
	def, ok := definitions[Opcode(op)]
	if !ok {
		return nil, fmt.Errorf("opcode %d undefined", op)
	}
	return def, nil
}

// Make takes a Opcode as the first argument then variadic operands follow.
// It looks up the value of Opcode in definitions map (note that it does NOT use Lookup function so we don't need to handle
// possible errors on every instruction. This risks creating empty byte slices however).
// It then allocates a byte slice with the correct length for that instruction and sets the Opcode as the first byte.
// Then it loops through the rest of the operands, encoding each one with the correct width.
// Resulting byte slice is returned to the caller.
func Make(op Opcode, operands ...int) []byte {
	// Confirm that the opcode is a known Opcode
	// We don't use the Lookup and therefore we don't have to deal with possible errors
	// This risks creating empty byte slices, but it's a risk we'll allow ourselves
	def, ok := definitions[op]
	if !ok {
		return []byte{}
	}

	// Lookup what the length of the resulting instruction
	instructionLen := 1
	for _, w := range def.OperandWidths {
		instructionLen += w
	}
	// Use that width from above
	instruction := make([]byte, instructionLen)
	// The first byte of an Instruction is always the Opcode
	instruction[0] = byte(op)

	offset := 1
	for i, o := range operands {
		width := def.OperandWidths[i]
		switch width {
		case 2:
			binary.BigEndian.PutUint16(instruction[offset:], uint16(o))
		}
		offset += width
	}
	return instruction
}

// ReadOperands is the opposite to Make.
// It returns the decoded operands and a number of bytes it read to decode the instruction.
func ReadOperands(def *Definition, ins Instructions) ([]int, int) {
	operands := make([]int, len(def.OperandWidths))
	offset := 0
	for i, width := range def.OperandWidths {
		switch width {
		case 2:
			operands[i] = int(ReadUint16(ins[offset:]))
		}
		offset += width
	}
	return operands, offset
}

// ReadUint16 is a simple helper method.
// It returns a uint16 integer from a received byte slice on type Instructions.
func ReadUint16(ins Instructions) uint16 {
	return binary.BigEndian.Uint16(ins)
}
