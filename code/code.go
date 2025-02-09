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

// Instructions consist of an Opcode (specifies the VM operation) and an arbitrary number of operands (0+).
// Opcode is always 1 byte long, operands can be multibyte.

const (
	// OpConstant is a ""pointer"" into the compilers constants pool
	OpConstant Opcode = iota
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

// definitions is a map of all known opcodes
var definitions = map[Opcode]*Definition{
	OpConstant: {"OpConstant", []int{2}},
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
