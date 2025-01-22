package evaluator

import (
	"fmt"
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
		return evalProgram(node.Statements)
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
		if isError(right) { // If this is an error, stop it from bubbling too far up. It should be returned here!
			return right
		}
		// Evaluate the actual operator, e.g. ! in !5, since we've already got the 5 as right in the lines above...
		return evalPrefixExpression(node.Operator, right)
	case *ast.InfixExpression: // As a reminder this has the specification of: <ast.Expression> <Operator> <ast.Expression>
		// We evaluate both the left and right Expression first via recursive calls to this Eval function...
		left := Eval(node.Left)
		if isError(left) {
			return left // If this is an error, stop it from bubbling too far up. It should be returned here!
		}
		right := Eval(node.Right)
		if isError(right) {
			return right // If this is an error, stop it from bubbling too far up. It should be returned here!
		}
		return evalInfixExpression(node.Operator, left, right)

	case *ast.BlockStatement:
		return evalBlockStatement(node)
	case *ast.IfExpression:
		return evalIfExpression(node)
	case *ast.ReturnStatement:
		val := Eval(node.ReturnValue) // Evaluate the return statement beforehand
		if isError(val) {             // If this is an error, stop it from bubbling too far up. It should be returned here!
			return val
		}
		return &object.ReturnValue{Value: val}
	}
	return nil
}

// evalProgram takes an slice of ast.Statement ([]ast.Statement).
// It evaluates every statement in Statements it got via a call to Eval and storing the result in temporary result
// variable (of type object.Object).
// It returns this variable after all the statements have been executed.
func evalProgram(statements []ast.Statement) object.Object {
	var result object.Object
	for _, statement := range statements {
		result = Eval(statement)
		switch result := result.(type) {
		case *object.ReturnValue:
			// Here, in evalProgram, we unwrap object.ReturnValue's, stopping program execution since we're on outer scope
			// We don't do that in evalBlockStatement
			return result.Value
		case *object.Error:
			return result // Also stop execution on an error, of course!
		}
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
// If the operator is unknown, a object.Error is returned to the caller instead!
func evalPrefixExpression(operator string, right object.Object) object.Object {
	switch operator {
	case "!":
		return evalBangOperatorExpression(right)
	case "-":
		return evalMinusPrefixOperatorExpression(right)
	default:
		return newError("unknown operator: %s%s", operator, right.Type())
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
// If evalMinusPrefixOperatorExpression receives a non object.Integer object.Object, a object.Error is returned to the caller!
// Similarly, if right parameter of type object.Object fails to cast to *object.Integer, a object.Error is returned to the caller!
// Otherwise, if everything went OK, a *object.Integer with negated value from right parameter is returned to the caller.
func evalMinusPrefixOperatorExpression(right object.Object) object.Object {
	if right.Type() != object.INTEGER_OBJ {
		return newError("unknown operator: -%s", right.Type())
	}
	integerLiteral, ok := right.(*object.Integer)
	if !ok {
		return newError("could not cast right (%s) to *object.Integer", right.Type())
	}
	value := integerLiteral.Value
	return &object.Integer{Value: -value}
}

// evalInfixExpression takes in a operator of type string, left and right of type object.Object.
// It performs a naked switch to decide how to parse this ast.InfixExpression.
// For example, if both left and right are object.INTEGER_OBJ, a call to evalIntegerInfixExpression is made and it's
// result is returned.
// As another example, when evaluating boolean expressions it's even simpler, it just calls and returns nativeBoolToBooleanObject
// which receives the argument of: <left> <native comparison operator> <right>.
// In case evalInfixExpression does not know how to parse the ast.InfixExpression a object.Error is returned to the caller!
// Or if there is a object type mismatch between left and right argument an object.Error is also returned to the caller!
func evalInfixExpression(operator string, left, right object.Object) object.Object {
	switch {
	case left.Type() != right.Type():
		return newError("type mismatch: %s %s %s", left.Type(), operator, right.Type())
	case left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ:
		return evalIntegerInfixExpression(operator, left, right)
	// Note that this is actually a pointer comparison.
	// It works on booleans and nulls because we have them defined just once in package and we reuse them for every true, false (and null) we encounter in the AST.
	// It wouldn't work with object.INTEGER_OBJ for example, because we allocate a new object for each integer literal we
	// encounter in the AST, i.e. 5 == 5 would actually give us false, because each integer would be it's own object,
	// pointers would not be the same, result would be false, which is NOT what we want.
	// This is also why evalIntegerInfixExpression has to come BEFORE these pointer comparison on boolean objects in the switch
	// statement. See the lines above, evalIntegerInfixExpression, and notice that boolean comparison are technically duplicated there.
	// It's also why, technically, boolean comparisons on booleans in Monkey language are FASTER then boolean integer comparisons.
	// With booleans we just compare pointers, with integers however have to UNWRAP THE VALUES AND COMPARE THEM!!!
	case operator == "==":
		return nativeBoolToBooleanObject(left == right)
	case operator == "!=":
		return nativeBoolToBooleanObject(left != right)
	default:
		return newError("unknown operator: %s %s %s", left.Type(), operator, right.Type())
	}
}

// evalIntegerInfixExpression evaluates ast.InfixExpression where both ast.InfixExpression.Left and ast.InfixExpression.Right
// are object.INTEGER_OBJ (ast.IntegerLiteral).
// If either or (left or right) is not a object.INTEGER_OBJ a object.Error is returned to the caller.
// If the operator is an unknown operator a object.Error is returned to the caller.
// Otherwise, a pointer to a new object.INTEGER_OBJ with the results of the specified operation is returned to the caller,
// if the operation is an operation which produces an integer (+, -, *, /).
// If the operation produces a boolean result a pointer to a new object.BOOLEAN_OBJ is returned.
func evalIntegerInfixExpression(operator string, left, right object.Object) object.Object {
	leftIntegerObj, ok := left.(*object.Integer)
	if !ok {
		return newError("could not cast left (of type %s) to integer", left.Type())
	}
	rightIntegerObj, ok := right.(*object.Integer)
	if !ok {
		return newError("could not cast right (of type %s) to integer", left.Type())
	}
	leftValue := leftIntegerObj.Value
	rightValue := rightIntegerObj.Value
	switch operator {
	case "+":
		return &object.Integer{Value: leftValue + rightValue}
	case "-":
		return &object.Integer{Value: leftValue - rightValue}
	case "*":
		return &object.Integer{Value: leftValue * rightValue}
	case "<":
		return nativeBoolToBooleanObject(leftValue < rightValue)
	case ">":
		return nativeBoolToBooleanObject(leftValue > rightValue)
	case "==":
		return nativeBoolToBooleanObject(leftValue == rightValue)
	case "!=":
		return nativeBoolToBooleanObject(leftValue != rightValue)
	case "/":
		return &object.Integer{Value: leftValue / rightValue}
	default:
		return newError("unknown operator: %s %s %s",
			left.Type(), operator, right.Type())
	}
}

// evalIfExpression accepts an pointer to a *ast.IfExpression.
// It then proceeds to evaluate the condition in the *ast.IfExpression.Condition via a recursive call to Eval.
// If the result is truthy (isTruthy), a ast.IfExpression.Consequence is evaluated via a recursive call to Eval and the result is
// returned to the caller.
// If the result of evaluated condition is not truthy, and there is an else branch (ast.IfExpression.Alternative),
// this alternative/else branch is evaluated via a recursive call to Eval and result is returned to the caller.
// Else, if condition is not truthy and there is no else/ast.IfExpression.Alternative, a NULL is returned to the caller!
func evalIfExpression(ie *ast.IfExpression) object.Object {
	condition := Eval(ie.Condition)
	if isError(condition) {
		return condition
	}
	if isTruthy(condition) {
		return Eval(ie.Consequence)
	} else if ie.Alternative != nil {
		return Eval(ie.Alternative)
	} else {
		return NULL
	}
}

// isTruthy evaluates weather object.Object is considered truthy in Monkey language.
// NULL and FALSE are considered to be falsy objects.
// TRUE or anything else is considered to be truthy.
// Note that isTruthy returns a native Go boolean and NOT a object.BOOLEAN_OBJ!
func isTruthy(obj object.Object) bool {
	switch obj {
	case NULL:
		return false
	case TRUE:
		return true
	case FALSE:
		return false
	default:
		return true
	}
}

// evalBlockStatement evaluates block statements {...}.
// The notable difference between evalBlockStatement and evalProgram is that evalBlockStatement does not unwrap
// object.ReturnValue, it instead checks if current statement is a object.ReturnValue via a call to object.ReturnValue.Type.
// It then returns the wrapped object.ReturnValue and not just the wrapped value like evalProgram does.
// This is done so that the object.ReturnValue is only unwrapped in evalProgram (where it stops further evaluation), which
// means that nested if's with returns should be parsed correctly.
func evalBlockStatement(block *ast.BlockStatement) object.Object {
	var result object.Object
	for _, statement := range block.Statements {
		result = Eval(statement)
		if result != nil {
			rt := result.Type()
			// Notice we don't unwrap returnValue object, we just return it back up
			// either way, both, an error and an return stop the execution of a statements in this block scope
			if rt == object.RETURN_VALUE_OBJ || rt == object.ERROR_OBJ {
				return result
			}
		}
	}
	return result
}

// newError creates a new object.Error with the given format and arguments (a), the error message, and returns a pointer to it.
func newError(format string, a ...any) *object.Error {
	return &object.Error{Message: fmt.Sprintf(format, a...)}
}

// isError is a simple helper function.
// It checks weather object.Object Type is a object.ERROR_OBJ.
// It returns a native Go boolean.
func isError(obj object.Object) bool {
	if obj != nil {
		return obj.Type() == object.ERROR_OBJ
	}
	return false
}
