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
	scopes          []CompilationScope // scopes is a stack of CompilationScope
	scopeIndex      int
}

// CompilationScope is used to track emitted instructions. Track last emitted instruction (lastInstruction) and the one
// before that (previousInstruction).
/*
For examples when compiling a function body:

1. We enter a new scope

2. We push a new scope into the Compiler scopes stack

3. Now whenever the compiler emits anything it should only modify the fields of the current CompilationScope.

4. Once we are done with a function body, we leave the scope, pop it from Compiler scopes stack and put the generated
instructions into a new *object.CompiledFunction
*/
type CompilationScope struct {
	instructions        code.Instructions  // instructions are the instructions which will encompass this scope
	lastInstruction     EmittedInstruction // lastInstruction is the last emitted instruction
	previousInstruction EmittedInstruction // previousInstruction is the instruction before lastInstruction
}

// enterScope creates and "enters" a new compiler scope. It does so by creating a new CompilationScope, appending it to
// the compiler scope stack (Compiler.scopes) and incrementing the compiler scope index (Compiler.scopeIndex).
// Note that enterScope also creates a new, enclosed, SymbolTable (NewEnclosedSymbolTable) and sets it as Compiler.symbolTable.
func (c *Compiler) enterScope() {
	scope := CompilationScope{
		instructions:        code.Instructions{},
		previousInstruction: EmittedInstruction{},
		lastInstruction:     EmittedInstruction{},
	}
	c.symbolTable = NewEnclosedSymbolTable(c.symbolTable) // Create a new enclosed symbol table with each scope.
	c.scopes = append(c.scopes, scope)
	c.scopeIndex++
}

// leaveScope "leaves" a CompilationScope by removing it from Compiler.scopes stack and decrementing the Compiler.scopeIndex.
// It also temporarily saves all the CompilationScope.instructions of the scope which is about to be removed, so they
// can be returned to the caller when the Compiler leaves the CompilationScope and leaveScope method returns.
// Note that leaveScope also resets the Compiler.symbolTable to the SymbolTable.Outer as it is leaving the scope.
func (c *Compiler) leaveScope() code.Instructions {
	instructions := c.currentInstructions()
	c.scopes = c.scopes[:len(c.scopes)-1]
	c.scopeIndex--
	c.symbolTable = c.symbolTable.Outer
	return instructions
}

// New returns a new initialized Compiler with a main scope initialized
func New() *Compiler {
	mainScope := CompilationScope{
		instructions:        code.Instructions{},
		lastInstruction:     EmittedInstruction{},
		previousInstruction: EmittedInstruction{},
	}
	symbolTable := NewSymbolTable()
	// Define the builtin objects/functions
	for i, v := range object.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}
	return &Compiler{
		instructions:    code.Instructions{},
		constants:       []object.Object{},
		lastInstruction: EmittedInstruction{},
		prevInstruction: EmittedInstruction{},
		symbolTable:     symbolTable,
		scopeIndex:      0,
		scopes:          []CompilationScope{mainScope},
	}
}

// currentInstructions returns the code.Instructions for the current scope (scopeIndex).
func (c *Compiler) currentInstructions() code.Instructions {
	return c.scopes[c.scopeIndex].instructions
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
		if c.lastInstructionIs(code.OpPop) {
			c.removeLastPop()
		}

		// Emit an `OpJump` with a bogus value
		jumpPos := c.emit(code.OpJump, 9999)

		// Now we know how big the consequence block is meaning we can fix up the garbage we generated above...
		afterConsequencePos := len(c.currentInstructions()) // The address after the consequence
		c.changeOperand(jneInsPos, afterConsequencePos)     // The OpJumpNotTruthy should NOT jump to 9999 anymore, it should jump to afterConsequencePos!

		if node.Alternative == nil {
			c.emit(code.OpNull) // If there is no alternative in this if statement we need to generate a *object.Null.
		} else {
			err := c.Compile(node.Alternative)
			if err != nil {
				return err
			}
			if c.lastInstructionIs(code.OpPop) {
				c.removeLastPop()
			}
		}
		afterAlternativePos := len(c.currentInstructions())
		c.changeOperand(jumpPos, afterAlternativePos)
	case *ast.BlockStatement:
		for _, statement := range node.Statements {
			err := c.Compile(statement)
			if err != nil {
				return err
			}
		}
	case *ast.LetStatement:
		// Define a symbol in the SymbolTable
		// Node.Name is the *ast.Identifier, the left side of the let statement
		// Therefore the node.Name.Value is the name of the Identifier, the string itself.
		// This has to be done before we compile the Value of the node in order for the function closures to be recursive
		symbol := c.symbolTable.Define(node.Name.Value)
		err := c.Compile(node.Value)
		if err != nil {
			return err
		}

		// Ask the SymbolTable what's the correct scope for this variable...
		if symbol.Scope == GlobalScope {
			c.emit(code.OpSetGlobal, symbol.Index) // put the symbol we created above into the bytecode as described in code/code.go
		} else {
			c.emit(code.OpSetLocal, symbol.Index)
		}

	case *ast.Identifier:
		// For every identifier we come across check if it's a "known" symbol otherwise we return a COMPILE time error...
		// In the evaluator we did this at runtime...Now we don't have to
		symbol, ok := c.symbolTable.Resolve(node.Value)
		if !ok {
			return errors.New("Undefined symbol: " + node.Value)
		}
		c.loadSymbol(symbol) // Emit the correct bytecode for the given symbol scope
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
	case *ast.FunctionLiteral:
		c.enterScope() // We need to create a new scope for this function since we want the bytecode to be "contained"

		// If the function has a name available, define it as a symbol
		if node.Name != "" {
			c.symbolTable.DefineFunctionName(node.Name)
		}
		for _, p := range node.Parameters { // Define all the function parameters in this scope
			c.symbolTable.Define(p.Value)
		}
		err := c.Compile(node.Body)
		if err != nil {
			return err
		}

		// If the last instruction in function body is a code.OpPop instruction then we need to replace this Pop with
		// code.OpReturnValue, since this is implied
		if c.lastInstructionIs(code.OpPop) {
			c.replaceLastPopWithReturn()
		}
		// On the other hand if there was no code.OpPop, no ast.ReturnStatement either then this means that we didn't have
		// any statements at all in the function body or only statements which we couldn't turn into an OpReturnValue.
		if !c.lastInstructionIs(code.OpReturnValue) {
			c.emit(code.OpReturn)
		}

		freeSymbols := c.symbolTable.FreeSymbols
		numLocals := c.symbolTable.numDefinitions // ask the SymbolTable how many [local] symbols were defined

		instructions := c.leaveScope() // Now we want to leave the scope and collect the instructions which were compiled in that scope

		for _, s := range freeSymbols {
			c.loadSymbol(s)
		}

		compiledFunction := &object.CompiledFunction{Instructions: instructions, NumLocals: numLocals, NumParameters: len(node.Parameters)} // So we can create the object we've talked about
		fnIndex := c.addConstant(compiledFunction)
		c.emit(code.OpClosure, fnIndex, len(freeSymbols)) // And push the compiled function object into the constants pool...
	case *ast.ReturnStatement:
		err := c.Compile(node.ReturnValue)
		if err != nil {
			return err
		}
		c.emit(code.OpReturnValue)
	case *ast.CallExpression: // All we need to do to compile a call expression, either bound to an identifier or literal is to compile the function and then emit an OpCall
		err := c.Compile(node.Function)
		if err != nil {
			return err
		}
		// Compile all the call arguments - They end up on the stack above the function we want to call
		for _, a := range node.Arguments {
			err := c.Compile(a)
			if err != nil {
				return err
			}
		}
		c.emit(code.OpCall, len(node.Arguments)) // When emitting also emit the number of function arguments
	}
	return nil
}

// loadSymbol emit (s) the correct bytecode for the given Symbol.Scope.
func (c *Compiler) loadSymbol(s Symbol) {
	switch s.Scope {
	case GlobalScope:
		c.emit(code.OpGetGlobal, s.Index)
	case LocalScope:
		c.emit(code.OpGetLocal, s.Index)
	case BuiltinScope:
		c.emit(code.OpGetBuiltin, s.Index)
	case FreeScope:
		c.emit(code.OpGetFree, s.Index)
	case FunctionScope:
		c.emit(code.OpCurrentClosure)

	}
}

// replaceLastPopWithReturn is used to handle implicit returns inside *object.CompiledFunction.
// It does so by replacing the last code.OpPop with code.OpReturnValue in that scope.
func (c *Compiler) replaceLastPopWithReturn() {
	lastPos := c.scopes[c.scopeIndex].lastInstruction.Position
	c.replaceInstruction(lastPos, code.Make(code.OpReturnValue))
	c.scopes[c.scopeIndex].lastInstruction.Opcode = code.OpReturnValue
}

// replaceInstruction replaces instruction in Compiler instructions pool at position pos with a new one, as the name might suggest...
func (c *Compiler) replaceInstruction(pos int, newInstruction []byte) {
	ins := c.currentInstructions()
	for i := 0; i < len(newInstruction); i++ {
		ins[pos+i] = newInstruction[i]
	}
}

// changeOperand changes just the operand of instruction at position opPos in Compiler instructions pool with a new one
// by calling replaceInstruction with reused OPCODE.
func (c *Compiler) changeOperand(opPos, operand int) {
	op := code.Opcode(c.currentInstructions()[opPos])
	newInstruction := code.Make(op, operand)
	c.replaceInstruction(opPos, newInstruction)
}

// lastInstructionIsPop is a simple helper method. It checks if the Compiler lastInstruction instruction was a code.OpPop.
// If it was, true is returned, false otherwise.
// **DEPRECATED** use lastInstructionIs instead with opcode of code.OpPop.
func (c *Compiler) lastInstructionIsPop() bool {
	return c.scopes[c.scopeIndex].lastInstruction.Opcode == code.OpPop
}

// lastInstructionIs firstly checks that there are any instruction at all in the current CompilationScope. If there aren't
// any false is returned.
// Otherwise it checks if the given Opcode is the same as Compiler.scopes[ Compiler.scopeIndex ].lastInstruction.Opcode.
func (c *Compiler) lastInstructionIs(op code.Opcode) bool {
	if len(c.currentInstructions()) == 0 {
		return false
	}
	return c.scopes[c.scopeIndex].lastInstruction.Opcode == op
}

// removeLastPop does some slicing on Compiler instructions pool. More specifically, it slices out the start position of
// lastInstruction. It then also makes sure that lastInstruction is still correctly set, i.e. it sets it to prevInstruction.
// As the name suggests, it's meant for removing the last code.OpPop, although it could remove any defined instruction technically...
func (c *Compiler) removeLastPop() {
	last := c.scopes[c.scopeIndex].lastInstruction
	previous := c.scopes[c.scopeIndex].previousInstruction
	old := c.currentInstructions()
	newIns := old[:last.Position]
	c.scopes[c.scopeIndex].instructions = newIns
	c.scopes[c.scopeIndex].lastInstruction = previous
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
	previous := c.scopes[c.scopeIndex].lastInstruction
	last := EmittedInstruction{Opcode: op, Position: pos}
	c.scopes[c.scopeIndex].previousInstruction = previous
	c.scopes[c.scopeIndex].lastInstruction = last
}

// addInstruction takes an byte slice which represents the instruction and adds it into the Compiler's instruction slice.
// It returns the (starting) position of where it was placed.
func (c *Compiler) addInstruction(ins []byte) int {
	posNewInstruction := len(c.currentInstructions())
	updatedInstructions := append(c.currentInstructions(), ins...)
	c.scopes[c.scopeIndex].instructions = updatedInstructions
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
		Instructions: c.currentInstructions(),
		Constants:    c.constants,
	}
}
