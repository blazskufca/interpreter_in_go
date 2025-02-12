package compiler

import (
	"errors"
	"github.com/blazskufca/interpreter_in_go/ast"
	"github.com/blazskufca/interpreter_in_go/code"
	"github.com/blazskufca/interpreter_in_go/object"
)

// Compiler has two internal fields which are modified by the Compiler.Compile method.
type Compiler struct {
	instructions code.Instructions // instructions holds the compiled bytecode.
	constants    []object.Object   // constants is a constants pool (https://en.wikipedia.org/wiki/Literal_pool)
}

// New returns a new initialized Compiler
func New() *Compiler {
	return &Compiler{
		instructions: code.Instructions{},
		constants:    []object.Object{},
	}
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
	}
	return nil
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
func (c *Compiler) emit(op code.Opcode, operands ...int) int {
	ins := code.Make(op, operands...)
	return c.addInstruction(ins)
}

// addInstruction takes an byte slice which represents the instruction and adds it into the Compiler's instruction slice.
// It returns the (starting) position of where it was placed.
func (c *Compiler) addInstruction(ins []byte) int {
	posNewInstruction := len(ins)
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
