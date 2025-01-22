package evaluator

import (
	"github.com/blazskufc/interpreter_in_go/ast"
	"github.com/blazskufc/interpreter_in_go/object"
)

// This is a small optimization of evaluator.
// If you think about it, there's no point in creating new object.Boolean each time we encounter a boolean in AST...
// We can just define TRUE and FALSE in as a global variable in the package, create one object.Boolean which represents
// true and one which is meant to represent false, and then reuse these two when we need them instead of creating a new
// object for each boolean node in the AST.
var (
	// TRUE creates object.Boolean object.Object with the value "true" as you might suspect.
	// It's meant to represent "true" booleans in monkey language AST.
	TRUE = &object.Boolean{Value: true}
	// FALSE creates object.Boolean object.Object with the value "false" as you might suspect.
	// It's meant to represent "false" booleans in monkey language AST.
	FALSE = &object.Boolean{Value: false}
	// NULL is an instance of object.Null.
	// Just like with TRUE and FALSE, there is no point in creating a new object.Null each time we encounter a null in monkey
	// lang AST.
	NULL = &object.Null{}
)

// Eval takes an node (ast.Node) which is an AST node as the package name suggests, evaluates it and returns appropriate
// object.Object.
// If it does not know which object.Object it should return it returns nil!
func Eval(node ast.Node) object.Object {
	switch node := node.(type) {
	case *ast.Program: // Evaluate all the statements in the program
		return evalStatements(node.Statements)
	case *ast.ExpressionStatement: // Evaluate the expression part of the ast.ExpressionStatement
		return Eval(node.Expression)
	case *ast.IntegerLiteral: // Evaluates ast.IntegerLiteral
		return &object.Integer{Value: node.Value}
	case *ast.Boolean: // Evaluates ast.Boolean
		// Small optimization -> Reuse already created objects instead of creating new ones
		return nativeBoolToBooleanObject(node.Value)
	case *ast.PrefixExpression: // e.g. "!5" - Everything besides let statement and return statement in Monkey is an ast.Expression
		// Evaluate the "right" part of the prefixExpression, e.g. The actual value, e.g. the "5" in !5
		right := Eval(node.Right)
		// Evaluate the actual operator, e.g. ! in !5, since we've already got the 5 as right in the lines above...
		return evalPrefixExpression(node.Operator, right)
	}
	return nil
}

// evalStatements takes an slice of ast.Statement ([]ast.Statement).
// It evaluates every statement in Statements it got via a call to Eval and storing the result in temporary result
// variable (of type object.Object).
// It returns this variable after all the statements have been executed.
func evalStatements(statements []ast.Statement) object.Object {
	var result object.Object
	for _, statement := range statements {
		result = Eval(statement)
	}
	return result
}

// nativeBoolToBooleanObject is a small optimization helper.
// It accepts "input" bool (which will probably come from ast.Node), and then returns a pointer to TRUE if input is true
// and FALSE otherwise.
// Note that TRUE and FALSE are object.Boolean
func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input {
		return TRUE
	}
	return FALSE
}

// evalPrefixExpression evaluates ast.PrefixExpression ast.Expression's, e.g. "!5",
// To do so, it accepts two parameters. Operator of type string, which is the actual operator in the ast.PrefixExpression.
// It also accepts right of type object.Object - This is meant to represent already pre-evaluated ast.PrefixExpression.Right ast.Expression.
// evalPrefixExpression makes calls to sub-functions which know how to evaluate the given operator...
// Result of this sub-function call is returned from this function to the caller.
// If the operator is unknown, a NULL is returned to the caller instead!
func evalPrefixExpression(operator string, right object.Object) object.Object {
	switch operator {
	case "!":
		return evalBangOperatorExpression(right)
	case "-":
		return evalMinusPrefixOperatorExpression(right)

	default:
		return NULL
	}
}

// evalBangOperatorExpression known how to handle ! operator in Monkey Language.
// For example, it knows that !5 should return FALSE (object.Integer's are truthy in Monkey Language)
// evalBangOperatorExpression only ever returns two things - TRUE or FALSE.
func evalBangOperatorExpression(right object.Object) object.Object {
	switch right {
	case TRUE: // !true = false
		return FALSE
	case FALSE: // !false = true
		return TRUE
	case NULL: // I guess you can think of this as "not None" in python? NULL is falsy in Monkey language
		return TRUE
	default:
		return FALSE
	}
}

// evalMinusPrefixOperatorExpression knows how to evaluate the - operator.
// Please note that only integer literals are allowed to have the minus prefix operator in Monkey.
// If evalMinusPrefixOperatorExpression receives a non object.Integer object.Object, a NULL is returned to the caller!
// Similarly, if right parameter of type object.Object fails to cast to *object.Integer, a NULL is returned to the caller!
// Otherwise, if everything went OK, a *object.Integer with negated value from right parameter is returned to the caller.
func evalMinusPrefixOperatorExpression(right object.Object) object.Object {
	if right.Type() != object.INTEGER_OBJ {
		return NULL
	}
	integerLiteral, ok := right.(*object.Integer)
	if !ok {
		return NULL
	}
	value := integerLiteral.Value
	return &object.Integer{Value: -value}
}
