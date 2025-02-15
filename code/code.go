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
/*
BUILDING ARRAYS:

Arrays are not constants! They are composite data types, i.e., they are composed out of other data types! (https://en.wikipedia.org/wiki/Composite_data_type)

Suppose we have the following monkey array (don't look too much into this being integer arithmetic, because that's not the
point; yes, we could pre-calculate this example at compile time and then pass a ""static"" array to the VM, but what if
these are not integers but functions? Then we can't do that!):

														[1 + 2, 3 + 4, 5 + 6]

We have to instead tell to VM to build an array and how to do so. - OpArray <array length>

1. We'll compile all the array elements
2. Compiling them leaves <array length> elements on the VM's stack
3. Emit OpArray afterward
4. When the VM executes the OpArray opcode, it collects the last <array length> elements off its stack, builds the
*object.Array and pushes it onto the stack!

Similar applies to hash literals:

													{1 + 1: 2 * 2, 3 + 3: 4 * 4}

That’s equivalent to this hash literal:

															{2: 4, 6: 16}

But not only do we have N dynamic values, we also have N dynamic keys.
*/
/*
INDEX OPERATOR:

The index operator is:
														<expression>[<expression>]

The data structure being indexed and the index itself can be produced by any expression. On semantic level this means that
the index operator could work with any object.Object, either as the index or the value, despite the fact that we only need them
for arrays and hash literals.

But despite the index operator being defined only on arrays and hash literals, we'll implement it in a generic way:
	1. There need to be two values sitting on the VM stack:
																index
														object_to_be_indexed
	2. VM takes both off the stack
	3. VM performs the index
	4. VM puts the result back onto the stack

*/
/*
COMPILING AND EXECUTING FUNCTIONS:

In the bytecode interpreter we'll be handling function in the following way:

1. We've defined a object.CompiledFunction which holds all the instruction of a function. This object will be passed to the
VM as a constant (OpConstant instruction) and will therefore be placed into compilers constants pool.

2. Function calls are instructed via a OpCall opcode. CompiledFunction has to be sitting on top of the stack in ordered for this to work.

3. We'll handle returns from a function in two ways:
	a. Implicit and explicit returns, i.e. when there is a value, are handled by OpReturnValue - This instruction instructs the
		monkey Virtual Machine to return the value on top of the stack to the calling context and resume program execution there

	b. Functions with no return values (which are hard to even create in monkey, but that's besides the point - they do
		exists) are handled by OpReturn. This instruction also tells the VM to transfer control back to the calling context
		but instead of returning the value on top of the function stack, return a vm.Null, because there is no return value!

Here is a quick diagram showcasing an example of a CompiledFunction object. Suppose we are compiling the following monkey
code:
														fn() { return 5 + 10 }

This would be a CompiledFunction object for the source code above:

											+------------------+------------------------------+
											|   OpConstant 0   | <--- Load 5 on to the stack  |
											+------------------+------------------------------+
											|   OpConstant 1   | <--- Load 10 on to the stack |
											+------------------+------------------------------+
											|      OpAdd       | <--- Add them together       |
											+------------------+------------------------------+
											| OpReturnValue    | <--- Return value on top     |
											|                  |      of stack                |
											+------------------+------------------------------+
*/
/*
LOCAL BINDINGS:

We know that local bindings come with a unique index, just like globals.

So we can use the operand of OpSetLocal as the index into the data structure. But which store? If we just put everything
into globals data structure then there is no point in having locals at all!

There are two possible options with which we could answer this question:

	1. We could dynamically allocate local bindings and store them in their own data structure, a slice or whatever.
		Everytime we need a new locals storage, we allocate a new empty slice and use it to work with the locals.

	2. If we think about it, we already have a place where we store function relevant data - The VM stack. So why not
		reuse it for locals too? It's a bit harder to implement, but as a bonus not only will we see how to do this in VM
		but we'll also gain more insight into low level operations of real machines, since this is how real machines store locals.

We'll be going with the second option and here is how it works:

	1. We come across an OpCall instruction - We are about to execute the object.CompiledFunction sitting on top of the stack,
		we store the stack pointer

	2. We increase the stack pointer by the number of locals used within the function
		- Result of this is a "hole" in the stack - We've incremented the stack pointer, but we did not push anything onto
			it.
		a.) Below this "hole" are all the values previously pushed onto the stack, i.e. values before the function call
		b.) Above the "hole" is stack which is available for use in the function itself - The function workspace
		c.) The "hole" itself is where we'll be storing the local bindings. We'll index into this "hole" instead of some
			outer data structure.
		- Excusing for some minor differences (like our stack growing up and real stack growing "down"), this is also
			how you would do it in x86_64 assembly:

													some_func:
														enter $16, $0		# Reserves 16 bytes for locals (hole) - 16 bytes because on real machines it a good idea to keep the stack aligned to multiples of 16
														movq $1, -8(%rbp)   # Stores 1 in the first local slot
														movq -8(%rbp), %rax	# Load the local into RAX register...Pointless, just to showcase
														leave 				# When finishing the function execution clean up the stack
														ret					# Return the RAX, which has the local in it, which is 1.

		- Here is a diagram for this idea of making "holes" in the stack to store locals

															+----------------+
															|                |
															+----------------+
															|                |
															+----------------+
												vm.sp -->   |                |
															+----------------+ -----+
															|    Local 2     |		|
															+----------------+		| -- reserved for locals
															|    Local 1     |     	|
															+----------------+ -----+
															|   Function     |
															+----------------+ -----+
															| Other Value 2  |		|
															+----------------+ 		| -- pushed before call
															| Other Value 1  |     	|
															+----------------+ -----+

	- A beauty of this approach is how we can "clean up" a function which has finished executing and is about to return!
		- Since we've saved the stack pointer before we started the execution we can now just restore it.
			- Not only will this clean up the "function workspace" but it will also remove all the locals
*/
/*
CALL ARGUMENTS:

Calling convention without function arguments is as follows:
	1. Push object.CompiledFunction onto the VM stack
	2. Emit an OpCall instruction

With function arguments the calling convention changes to:
	1. Push the object.CompiledFunction you want to call
	2. Push the arguments for the function onto the stack
	3. Emit an OpCall instruction

Here is the diagram of the stack when setting up a function call with the new calling convention:

													   +----------------+
													   |                |
													   +----------------+
													   |                |
													   +----------------+
										vm.sp ------>  |                |
													   +----------------+
													   |     Arg 2      |
													   +----------------+
													   |     Arg 1      |
													   +----------------+
													   |   Function     |
													   +----------------+

Or rather, more correct diagram, accounting for Frames and base pointer, is this:

													   +----------------+
													   |                |
													   +----------------+
													   |                |
													   +----------------+
										vm.sp ------>  |                | <--- basePointer + 2
													   +----------------+
													   |     Arg 2      | <--- basePointer + 1
													   +----------------+
													   |     Arg 1      | <--- basePointer
													   +----------------+
													   |   Function     |
													   +----------------+
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
	OpArray                       // OpArray encodes the instruction which instructs the vm.VM to dynamically build an array based on the operand value
	OpHash                        // OpHash instruct the monkey vm.VM to build a dynamic hash literal
	OpIndex                       // OpIndex represents the index operator in bytecode
	OpCall                        // OpCall is a bytecode representation of a function call
	OpReturnValue                 // OpReturnValue represents both explicit and implicit returns from a function in bytecode
	OpReturn                      // OpReturn signifies a return from a Monkey function with NO return value (no implicit and no explicit return)
	OpGetLocal                    // OpGetLocal instructs the vm.VM to load the value from locals store and load it onto the stack
	OpSetLocal                    // OpSetLocal instructs the vm.VM to pop the topmost stack value and store it into locals store at specified index
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

ARRAYS:

OpArray: Causes the Monkey vm.VM to collect the N elements off its stack, build an *object.Array and then push this array
back onto its stack! It has a single 2 byte operand, which is the value of N - The length of the array or rather, how many
previous values to collect when building the array! The maximum size of N and therefore arrays in Monkey language is
65535.

HASH LITERALS:

OpHash: Similarly to array literals, hash literals are constructed dynamically by the vm.VM from the last <number_of_elements>
on the stack. OpHash accepts a single 2 byte operand, which is the <number_of_elements> value. Maximum length of the
key-value pairs in a hash literal is 65535, due to this being a 16-bit operand.

INDEX OPERATOR:

OpIndex: Instructs the vm.VM to take the topmost object.Object off the stack and use it as the index and then take
the object.Object coming after and use it as the structure to be indexed into. Result is pushed back onto the stack.
OpIndex has no operands.

FUNCTIONS:

OpCall: Represents a function call (causes the vm.VM to execute a call of specific function literal). It has a single
1-byte operand, representing the number of arguments this function will receive and have been pushed onto the vm.VM
stack for the function to use. Since the operand is a single byte, number of function arguments is limited to 256.

OpReturnValue: Instructs the monkey vm.VM to return from a function with a return value (it represents both implicit and
explicit returns). The value which will be return is sitting/has to sit on top of the VM stack. OpReturnValue has no operands.

OpReturn: Instructs the Monkey vm.VM to return from a function with no return value (no explicit return value and no
implicit return value). In Monkey lack of value is displayed by object.Null (*vm.Null package global rather).
Note that it's hard to come up with such a function at all in Monkey, but they do exist. Here are two
examples of such functions:

												fn() { } // Example 1

And:

												fn() { let a = 1; } // Example 2

OpReturn is meant to instruct the vm.VM on how to correctly handle such functions. OpReturn opcode has no operands.

OpGetLocal: Instructs the vm.VM to load a value from locals store at the specified index onto the vm.VM stack. It has a
single 1 byte operand, which is the index of the value to load in the locals store. Since operand is a single byte operand
the maximum numbers of locals a scope can have is 256!

OpSetLocal: Instructs the vm.VM to store topmost stack  value into locals store at the specified index. It has a
single 1 byte operand, which is the index under which the value should be stored in the locals store. Since operand is a single byte operand
the maximum numbers of locals you can have is 256!
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
	OpArray:         {"OpArray", []int{2}},
	OpHash:          {"OpHash", []int{2}},
	OpIndex:         {"OpIndex", []int{}},
	OpCall:          {"OpCall", []int{1}},
	OpReturnValue:   {"OpReturnValue", []int{}},
	OpReturn:        {"OpReturn", []int{}},
	OpGetLocal:      {"OpGetLocal", []int{1}},
	OpSetLocal:      {"OpSetLocal", []int{1}},
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
		case 1:
			instruction[offset] = byte(o)
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
		case 1:
			operands[i] = int(ReadUint8(ins[offset:]))
		}
		offset += width
	}
	return operands, offset
}

// ReadUint8 is a simple helper method. It reads a single byte instructions.
func ReadUint8(ins Instructions) uint8 { return uint8(ins[0]) }

// ReadUint16 is a simple helper method.
// It returns a uint16 integer from a received byte slice on type Instructions.
func ReadUint16(ins Instructions) uint16 {
	return binary.BigEndian.Uint16(ins)
}
