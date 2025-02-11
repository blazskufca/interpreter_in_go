package compiler

import (
	"errors"
	"github.com/blazskufc/interpreter_in_go/ast"
	"github.com/blazskufc/interpreter_in_go/code"
	"github.com/blazskufc/interpreter_in_go/object"
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
	case *ast.InfixExpression:
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
		default:
			return errors.New("Unsupported operator: " + node.Operator)
		}
	// Because literals are constant, we can evaluate them right here.
	// And by "evaluate", to omit the fancy words, we just mean create a *object.Integer from our object system
	// Because this is a literal, we add it into a constant pool
	case *ast.IntegerLiteral:
		integer := &object.Integer{Value: node.Value}
		c.emit(code.OpConstant, c.addConstant(integer))
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
