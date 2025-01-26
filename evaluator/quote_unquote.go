package evaluator

import (
	"github.com/blazskufc/interpreter_in_go/ast"
	"github.com/blazskufc/interpreter_in_go/object"
	"github.com/blazskufc/interpreter_in_go/token"
	"strconv"
)

// quote is quote function of the Macro system in Monkey.
// Its purpose is to stop evaluation and return an ast.Node instead!
// And that's exactly what it does. quote returns object.Quote with the object.Quote.Node being the node from the
// function argument.
// It accepts an *object.Environment which then passed to evalUnquoteCalls which evaluates the unquoted environment.
func quote(node ast.Node, env *object.Environment) object.Object {
	// This handle's all the unquote macros, i.e. quote skips evaluation, unquote evaluates again
	node = evalUnquoteCalls(node, env)
	return &object.Quote{Node: node}
}

// evalUnquoteCalls evaluate things inside the unquote macro...
// It means it replaces unquote ast.Node's with evaluated versions of the nodes.
func evalUnquoteCalls(quoted ast.Node, env *object.Environment) ast.Node {
	return ast.Modify(quoted, func(node ast.Node) ast.Node {
		// If it's not a unquote call we shall just return the node, there's nothing to do
		if !isUnquoteCall(node) {
			return node
		}
		// unquote should always be a call expressions...Look into object and ast, should make sense there!
		call, ok := node.(*ast.CallExpression)
		if !ok {
			return node
		}
		// We only allow a single argument to unquote call
		if len(call.Arguments) != 1 {
			return node
		}
		// This would be sick if it worked like so...Just call the Eval with environment passed down and return evaluated node
		// Except it won't work because Eval returns *object.Object and we need a ast.Node... We're returning something
		// completely different...
		// return Eval(call.Arguments[0], env)
		// instead we have to convert it back into ast.node
		return convertObjectToASTNode(Eval(call.Arguments[0], env))
	})
}

// isUnquoteCall check's that node is firstly a *ast.CallExpression and the token literal in this *ast.CallExpression
// is an "unquote" token...
// Returns a boolean, true indicating that the node is unquoted *ast.CallExpression and false indicating it's something else.
func isUnquoteCall(node ast.Node) bool {
	callExpression, ok := node.(*ast.CallExpression)
	if !ok {
		return false
	}
	return callExpression.Function.TokenLiteral() == "unquote"
}

// convertObjectToASTNode converts (evaluated) object.Object's back into AST ast.Node.
// At first sight it might not make much sense why this switch would be needed, but it is needed inside the Macro system.
// Specifically inside evalUnquoteCalls ast.ModifierFunc, which must evaluate unquoted parameters but it must also return
// ast.Node and not object.Object...
func convertObjectToASTNode(obj object.Object) ast.Node {
	switch obj := obj.(type) {
	case *object.Integer:
		t := token.Token{
			Type:    token.INT,
			Literal: strconv.FormatInt(obj.Value, 10),
		}
		return &ast.IntegerLiteral{Token: t, Value: obj.Value}
	case *object.Boolean:
		var t token.Token
		if obj.Value {
			t = token.Token{
				Type:    token.TRUE,
				Literal: "true",
			}
		} else {
			t = token.Token{
				Type:    token.FALSE,
				Literal: "false",
			}
		}
		return &ast.Boolean{Token: t, Value: obj.Value}
	// This case allows us to handle things like this:
	// quote(unquote(quote(4 + 4))) --> produces --> (4 + 4)
	// let quotedInfixExpression = quote(4 + 4)
	// quote(unquote(4 + 4) + unquote(quotedInfixExpression)) --> produces --> (8 + (4 + 4))
	case *object.Quote:
		return obj.Node
	default:
		return nil
	}
}
