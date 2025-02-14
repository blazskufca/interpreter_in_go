package compiler

import (
	"errors"
	"github.com/blazskufca/interpreter_in_go/ast"
	"github.com/blazskufca/interpreter_in_go/code"
	"github.com/blazskufca/interpreter_in_go/object"
	"sort"
)

// EmittedInstruction tracks code.Opcode instruction and it's position in the stream
type EmittedInstruction struct {
	Opcode   code.Opcode
	Position int
}

// Compiler has two internal fields which are modified by the Compiler.Compile method.
type Compiler struct {
	instructions    code.Instructions  // instructions holds the compiled bytecode.
	constants       []object.Object    // constants is a constants pool (https://en.wikipedia.org/wiki/Literal_pool)
	lastInstruction EmittedInstruction // lastInstruction is the very last instruction we've emitted
	prevInstruction EmittedInstruction // previousInstruction is the instruction which came before lastInstruction
	symbolTable     *SymbolTable       // *SymbolTable is a pointer to the SymbolTable, which holds bound information about identifiers (Symbol)
}

// New returns a new initialized Compiler
func New() *Compiler {
	return &Compiler{
		instructions:    code.Instructions{},
		constants:       []object.Object{},
		lastInstruction: EmittedInstruction{},
		prevInstruction: EmittedInstruction{},
		symbolTable:     NewSymbolTable(),
	}
}

// NewWithState returns a pointer to a new Compiler with preset SymbolTable and constants.
func NewWithState(s *SymbolTable, constants []object.Object) *Compiler {
	compiler := New()
	compiler.symbolTable = s
	compiler.constants = constants
	return compiler
}

// Compile recursively walks the AST, compiling every node as it goes along.
func (c *Compiler) Compile(node ast.Node) error {
	switch node := node.(type) {
	case *ast.Program:
		for _, statement := range node.Statements {
			err := c.Compile(statement)
			if err != nil {
				return err
			}
		}
	case *ast.ExpressionStatement:
		err := c.Compile(node.Expression)
		if err != nil {
			return err
		}
		// This causes the compiler to emit code.OpPop bytecode instruction after every expressionStatement which in turn
		// causes the VM to pop it of the stack.
		// If this would not be here, the VM would be in very high risk of a stack overflow since expressions which have already
		// expired their lifetime would not be popped of the stack...potentially causing it to fill up very quickly
		_ = c.emit(code.OpPop)
	case *ast.InfixExpression:
		// We don't have a < in the bytecode in order to keep it "small" (and more so to showcase whats possible with
		// which was not possible with in-place evaluation)
		// To fix the ""missing"" less than operator we'll do the following
		if node.Operator == "<" {
			err := c.Compile(node.Right) // Compile the right node first instead of left one
			if err != nil {
				return err
			}
			err = c.Compile(node.Left) // Now compile the left node afterward
			if err != nil {
				return err
			}
			// We effectively reordered the bytecode and with that implemented the < less than operator, since
			// 1 < 2 is equal to 2 > 1 if you just reorder the boolean expression
			// We couldn't do that with our in-place eval
			c.emit(code.OpGreaterThan)
			return nil
		}
		err := c.Compile(node.Left)
		if err != nil {
			return err
		}
		err = c.Compile(node.Right)
		if err != nil {
			return err
		}
		switch node.Operator {
		case "+":
			c.emit(code.OpAdd)
		case "-":
			c.emit(code.OpSub)
		case "*":
			c.emit(code.OpMul)
		case "/":
			c.emit(code.OpDiv)
		case ">":
			c.emit(code.OpGreaterThan)
		case "==":
			c.emit(code.OpEqual)
		case "!=":
			c.emit(code.OpNotEqual)
		default:
			return errors.New("Unsupported operator: " + node.Operator)
		}
	// Because literals are constant, we can evaluate them right here.
	// And by "evaluate", to omit the fancy words, we just mean create a *object.Integer from our object system
	// Because this is a literal, we add it into a constant pool
	case *ast.IntegerLiteral:
		integer := &object.Integer{Value: node.Value}
		c.emit(code.OpConstant, c.addConstant(integer))
	case *ast.Boolean:
		if node.Value {
			c.emit(code.OpTrue)
		} else {
			c.emit(code.OpFalse)
		}
	case *ast.PrefixExpression:
		err := c.Compile(node.Right)
		if err != nil {
			return err
		}
		switch node.Operator {
		case "!": // The Bang prefix operator is defined for boolean arithmetic/operands
			c.emit(code.OpBang)
		case "-": // The Minus prefix operator is defined for integer arithmetic/operands
			c.emit(code.OpMinus)
		default:
			return errors.New("unsupported operator: " + node.Operator)
		}
	case *ast.IfExpression:
		err := c.Compile(node.Condition)
		if err != nil {
			return err
		}
		// We know we need to jump SOMEWHERE if the compiled condition above is Falsy with code.OpJumpNotTruthy...
		// But where?? What's the address of the instruction after the consequence block???
		// That's a fair question because, to be truthful, we don't know! So how about if we just put garbage into it for
		// now and worry about it later? Sounds like a joke, but it's not!
		// We save the position of this (at the moment) bogus instruction for what follows next...
		jneInsPos := c.emit(code.OpJumpNotTruthy, 9999) // <- OpJumpNotTruthy with garbage operand!
		err = c.Compile(node.Consequence)
		if err != nil {
			return err
		}
		// We SHOULD get rid of the code.OpPop instruction generated by compiling code.Consequence expression, because other we couldn't do this in Monkey:
		// let result = if (5 > 3) { 5 } else { 3 };
		// since the value generated by consequence would be popped of the VMs stack and there wouldn't be anything on the right side of "="...
		// We need the remove only the last code.OpPop...Suppose we have this monkey code now:
		// if (true) {3; 2; 1;}
		// In this case only the value "1" should remain on the VMs stack, but other values (3, 2) SHOULD be popped off!
		if c.lastInstructionIsPop() {
			c.removeLastPop()
		}

		// Emit an `OpJump` with a bogus value
		jumpPos := c.emit(code.OpJump, 9999)

		// Now we know how big the consequence block is meaning we can fix up the garbage we generated above...
		afterConsequencePos := len(c.instructions)      // The address after the consequence
		c.changeOperand(jneInsPos, afterConsequencePos) // The OpJumpNotTruthy should NOT jump to 9999 anymore, it should jump to afterConsequencePos!

		if node.Alternative == nil {
			c.emit(code.OpNull) // If there is no alternative in this if statement we need to generate a *object.Null.
		} else {
			err := c.Compile(node.Alternative)
			if err != nil {
				return err
			}
			if c.lastInstructionIsPop() {
				c.removeLastPop()
			}
		}
		afterAlternativePos := len(c.instructions)
		c.changeOperand(jumpPos, afterAlternativePos)
	case *ast.BlockStatement:
		for _, statement := range node.Statements {
			err := c.Compile(statement)
			if err != nil {
				return err
			}
		}
	case *ast.LetStatement:
		err := c.Compile(node.Value)
		if err != nil {
			return err
		}
		// Define a symbol in the SymbolTable
		// Node.Name is the *ast.Identifier, the left side of the let statement
		// Therefore the node.Name.Value is the name of the Identifier, the string itself.
		symbol := c.symbolTable.Define(node.Name.Value)
		c.emit(code.OpSetGlobal, symbol.Index) // put the symbol we created above into the bytecode as described in code/code.go
	case *ast.Identifier:
		// For every identifier we come across check if it's a "known" symbol otherwise we return a COMPILE time error...
		// In the evaluator we did this at runtime...Now we don't have to
		symbol, ok := c.symbolTable.Resolve(node.Value)
		if !ok {
			return errors.New("Undefined symbol: " + node.Value)
		}
		// If we have the symbol we can emit a OpGetGlobal to cause the VM to stack load it
		c.emit(code.OpGetGlobal, symbol.Index)
	case *ast.StringLiteral:
		str := &object.String{Value: node.Value}
		c.emit(code.OpConstant, c.addConstant(str))
	case *ast.ArrayLiteral:
		// Compile all the elements of the array like we've talked about in code/code.go
		for _, element := range node.Elements {
			err := c.Compile(element)
			if err != nil {
				return err
			}
		}

		c.emit(code.OpArray, len(node.Elements)) // emit an code.OpArray which will case the VM to build the array dynamically!
	case *ast.HashLiteral:
		var keys []ast.Expression
		for k := range node.Pairs {
			keys = append(keys, k)
		}
		// Keys is a map[ast.Expression]ast.Expression and in go maps are not stable, i.e. iterating over them is not
		// guaranteed to give a specific order, so we need to sort it manually...Meaning we would get back compiled bytecode in
		// random orders...Which is not a huge deal, except for the tests, which would break because they expect specific order.
		sort.Slice(keys, func(i, j int) bool {
			return keys[i].String() < keys[j].String()
		})
		for _, k := range keys {
			err := c.Compile(k) // Compile the key - It's important that the keys are compiled first, because the VM will need to reconstruct it
			if err != nil {
				return err
			}
			err = c.Compile(node.Pairs[k]) // Compile the Value
			if err != nil {
				return err
			}
		}
		c.emit(code.OpHash, len(node.Pairs)*2)
	case *ast.IndexExpression:
		err := c.Compile(node.Left) // We first compile the structure being index into
		if err != nil {
			return err
		}
		err = c.Compile(node.Index) // Then we compile the index
		if err != nil {
			return err
		}
		c.emit(code.OpIndex) // Then we just emit a code.OpIndex
	}
	return nil
}

// replaceInstruction replaces instruction in Compiler instructions pool at position pos with a new one, as the name might suggest...
func (c *Compiler) replaceInstruction(pos int, newInstruction []byte) {
	for i := 0; i < len(newInstruction); i++ {
		c.instructions[pos+i] = newInstruction[i]
	}
}

// changeOperand changes just the operand of instruction at position opPos in Compiler instructions pool with a new one
// by calling replaceInstruction with reused OPCODE.
func (c *Compiler) changeOperand(opPos, operand int) {
	OPCODE := code.Opcode(c.instructions[opPos])
	newInstruction := code.Make(OPCODE, operand)
	c.replaceInstruction(opPos, newInstruction)
}

// lastInstructionIsPop is a simple helper method. It checks if the Compiler lastInstruction instruction was a code.OpPop.
// If it was, true is returned, false otherwise.
func (c *Compiler) lastInstructionIsPop() bool {
	return c.lastInstruction.Opcode == code.OpPop
}

// removeLastPop does some slicing on Compiler instructions pool. More specifically, it slices out the start position of
// lastInstruction. It then also makes sure that lastInstruction is still correctly set, i.e. it sets it to prevInstruction.
// As the name suggests, it's meant for removing the last code.OpPop, although it could remove any defined instruction technically...
func (c *Compiler) removeLastPop() {
	c.instructions = c.instructions[:c.lastInstruction.Position]
	c.lastInstruction = c.prevInstruction
}

// addConstant adds a constant (literal expressions who's values don't change) of type object.Object to the
// Compiler constants pool.
// After this literal is added, a index of it in the compilers constant pool is returned to the caller, creating a unique
// identifier and a way to find it in the pool.
// This identifier is mainly used by the Monkey virtual machine in order to load the object/literal/constant onto its stack.
func (c *Compiler) addConstant(obj object.Object) int {
	c.constants = append(c.constants, obj)
	return len(c.constants) - 1
}

// Emit generates a code.Instructions by using code.Make and then adds it to the compilers instruction pool by calling
// addInstruction. This call generates (starting) position (integer) of this instruction in the Compiler's instructions pool which is
// then passed down/returned to the caller of the emit method.
// It also calls setLastInstruction which sets the Compiler lastInstruction and prevInstruction.
func (c *Compiler) emit(op code.Opcode, operands ...int) int {
	ins := code.Make(op, operands...)
	pos := c.addInstruction(ins)
	c.setLastInstruction(op, pos)
	return pos
}

// setLastInstruction sets the Compiler lastInstruction and prevInstruction.
// lastInstruction is the instruction which was just emitted and prevInstruction is the instruction which came before the
// lastInstruction.
func (c *Compiler) setLastInstruction(op code.Opcode, pos int) {
	previous := c.lastInstruction
	last := EmittedInstruction{
		Opcode:   op,
		Position: pos,
	}
	c.prevInstruction = previous
	c.lastInstruction = last
}

// addInstruction takes an byte slice which represents the instruction and adds it into the Compiler's instruction slice.
// It returns the (starting) position of where it was placed.
func (c *Compiler) addInstruction(ins []byte) int {
	posNewInstruction := len(c.instructions)
	c.instructions = append(c.instructions, ins...)
	return posNewInstruction
}

// Bytecode is a struct of compiler generated code.Instructions and evaluated constants.
// Bytecode is what's eventually passed to the Monkey virtual machine.
type Bytecode struct {
	Instructions code.Instructions // Instructions is a byte slice (code.Instructions) which the compiler has generated and will be passed to the VM.
	Constants    []object.Object   // Constants is a poll of constants which the Compiler has evaluated.
}

// Bytecode on a Compiler type takes it's Compiler.instructions and Compiler.constants fields, wraps it into a Bytecode
// structure and returns a pointer to this structure.
func (c *Compiler) Bytecode() *Bytecode {
	return &Bytecode{
		Instructions: c.instructions,
		Constants:    c.constants,
	}
}
