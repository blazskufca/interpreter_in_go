package evaluator

import (
	"github.com/blazskufc/interpreter_in_go/ast"
	"github.com/blazskufc/interpreter_in_go/object"
)

// DefineMacros does two things.
// 1. Finds macro definitions
// 2. Removes them from the AST
func DefineMacros(program *ast.Program, env *object.Environment) {
	var definitions []int
	// Go through all the program statements
	// Because we only roll through program.Statements here this means we ONLY allow macros definitions on TOP LEVEL. We don't check
	// child nodes...
	for i, statement := range program.Statements {
		if isMacroDefinition(statement) { // If it is a macro definition
			addMacro(statement, env)
			definitions = append(definitions, i) // Keep track of the Index where this macro was found
		}
	}
	// Loop through all the macro definition indexes
	for i := len(definitions) - 1; i >= 0; i-- {
		definitionIndex := definitions[i]
		program.Statements = append( // Remove it by slicing ast.Program around the found definition.
			program.Statements[:definitionIndex],
			program.Statements[definitionIndex+1:]...,
		)
	}
}

// isMacroDefinition is a simple helper function. It receives a node.
// It firstly checks if this is a *ast.LetStatement (which it has to be). If it's not, it returns false.
// If it is a *ast.LetStatement, it is  then checked if it is also a *ast.MacroLiteral (which it also has to be).
// Boolean result of this check is returned to the caller.
/*
Despite the simplicity of the function, it holds a lot of power!
It defines what a valid macro definition is and what isn't.

As an example, consider this:

let myMacro = macro(x) { x };

let anotherNameForMyMacro = myMacro; // not valid currently

As this function is currently, second let statement is not a valid macro definition!
*/
func isMacroDefinition(node ast.Statement) bool {
	letStatement, ok := node.(*ast.LetStatement)
	if !ok {
		return false
	}
	_, ok = letStatement.Value.(*ast.MacroLiteral)
	return ok
}

// addMacro adds a macro definition to the passed environment.
// Note that it does not handle errors on type asserts, since this is what isMacroDefinition should have already checked
// and handled, therefore these two functions should either follow each other OR error handling should be explicitly added to
// addMacro!
func addMacro(statement ast.Statement, env *object.Environment) {
	letStatement, _ := statement.(*ast.LetStatement)
	macroLiteral, _ := letStatement.Value.(*ast.MacroLiteral)
	macro := &object.Macro{
		Parameters: macroLiteral.Parameters,
		Body:       macroLiteral.Body,
		Env:        env,
	}
	env.Set(letStatement.Name.Value, macro)
}

// ExpandMacros expands and evaluates the macros in the Environment. It uses ast.Modify to recursively walk the AST find
// calls to macros. If it finds a ast.Node which is a *ast.CallExpression which involves a macro it evaluates the call.
// Result of the evaluation (which must be *object.Quote) is returned to the caller or the function panics otherwise, if
// return is not a *object.Quote!
func ExpandMacros(program ast.Node, env *object.Environment) ast.Node {
	return ast.Modify(program, func(node ast.Node) ast.Node {
		callExpression, ok := node.(*ast.CallExpression)
		if !ok {
			return node
		}
		macro, ok := isMacroCall(callExpression, env)
		if !ok {
			return node
		}
		args := quoteArgs(callExpression)
		evalEnv := extendMacroEnv(macro, args)
		evaluated := Eval(macro.Body, evalEnv)
		quote, ok := evaluated.(*object.Quote)
		if !ok {
			panic("we only support returning AST-nodes from macros")
		}
		return quote.Node
	})
}

// isMacroCall is a simple helper function. It receives *ast.CallExpression and an *object.Environment.
// It then type asserts *ast.CallExpression.Function into *ast.Identifier. If it fails to do so, nil, false is returned
// to the caller.
// Otherwise, it get's the *ast.Identifier.Value from *object.Environment passed into the function. If it can't find it,
// nil, false is returned to the caller.
// It then asserts that whatever was bound to Identifier value in the Environment is a *object.Macro. If it's not, a false
// nil is returned to the caller.
// Otherwise said macro and true are returned to the caller!
func isMacroCall(callExpression *ast.CallExpression, env *object.Environment) (*object.Macro, bool) {
	identifier, ok := callExpression.Function.(*ast.Identifier)
	if !ok {
		return nil, false
	}
	obj, ok := env.Get(identifier.Value)
	if !ok {
		return nil, false
	}
	macro, ok := obj.(*object.Macro)
	if !ok {
		return nil, false
	}
	return macro, true
}

// quoteArgs is a simple helper function. It takes a *ast.CallExpression, loops through the arguments in this call
// expression, turns each one into *object.Quote and appends it to the []*object.Quote slice.
// This slice is then returned to the caller.
func quoteArgs(expression *ast.CallExpression) []*object.Quote {
	var args []*object.Quote
	for _, arg := range expression.Arguments {
		args = append(args, &object.Quote{Node: arg})
	}
	return args
}

// extendMacroEnv does what the name suggest. It extends the macro Environment.
// Of note, args are []*object.Quote created by quoteArgs most likely, and this is set in the Environment.
// Pointer to the extended Environment is returned to the caller!
func extendMacroEnv(macro *object.Macro, args []*object.Quote) *object.Environment {
	extendedEnv := object.NewEnclosedEnvironment(macro.Env)
	for paramIdx, param := range macro.Parameters {
		extendedEnv.Set(param.Value, args[paramIdx])
	}
	return extendedEnv
}
