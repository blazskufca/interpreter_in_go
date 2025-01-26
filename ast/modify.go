package ast

// ModifierFunc types take in a ast.Node, modify it, and return out a new, changed, ast.Node!
type ModifierFunc func(Node) Node

// Modify take in a node and applies the given ModifierFunc to it.
// Note that's it makes recursive calls to itself, because it has to handle nodes like *ast.Program and *ast.Expression,
// which contain children.
func Modify(node Node, modifier ModifierFunc) Node {
	switch node := node.(type) {
	case *Program:
		for i, statement := range node.Statements {
			node.Statements[i], _ = Modify(statement, modifier).(Statement)
		}
	case *ExpressionStatement:
		node.Expression, _ = Modify(node.Expression, modifier).(Expression)
	case *InfixExpression:
		node.Left, _ = Modify(node.Left, modifier).(Expression)
		node.Right, _ = Modify(node.Right, modifier).(Expression)
	case *PrefixExpression:
		// See the definition as to why this makes sense, we need to modify the right part
		node.Right, _ = Modify(node.Right, modifier).(Expression)
	case *IndexExpression:
		// Index and Left are both expressions in ast.Expression statement
		node.Index, _ = Modify(node.Index, modifier).(Expression)
		node.Left, _ = Modify(node.Left, modifier).(Expression)
	// ast.IfExpressions have quite a few moving parts all of which we need to modify.
	// They have Condition, they have Consequence, Alternative, and finally they have a ast.BlockStatement
	case *IfExpression:
		node.Condition, _ = Modify(node.Condition, modifier).(Expression)
		node.Consequence, _ = Modify(node.Consequence, modifier).(*BlockStatement)
		if node.Alternative != nil {
			node.Alternative, _ = Modify(node.Alternative, modifier).(*BlockStatement)
		}
	// ast.BlockStatement have an arbitrary number of *ast.Statements all of which have to be modified.
	case *BlockStatement:
		for i, _ := range node.Statements {
			node.Statements[i] = Modify(node.Statements[i], modifier).(Statement)
		}
	// Return statements have one child: the ReturnValue, which is an ast.Expression.
	case *ReturnStatement:
		node.ReturnValue = Modify(node.ReturnValue, modifier).(Expression)
	// Let statements also only have one moving part: the Value they’re binding to a name.
	case *LetStatement:
		node.Value, _ = Modify(node.Value, modifier).(Expression)
	// Function literals have a Body, which is an *ast.BlockStatement, and Parameters, which are a slice of *ast.Identifiers.
	case *FunctionLiteral:
		for i, _ := range node.Parameters {
			node.Parameters[i], _ = Modify(node.Parameters[i], modifier).(*Identifier)
		}
		node.Body, _ = Modify(node.Body, modifier).(*BlockStatement)
	// Array literals are comma-separated lists of expressions.
	// So we just have to loop through them and call modifier on each one, then reassign them back
	case *ArrayLiteral:
		for i, _ := range node.Elements {
			node.Elements[i] = Modify(node.Elements[i], modifier).(Expression)
		}
	// We have to modify each key-value pair, because they could contain the desired values
	case *HashLiteral:
		newPairs := make(map[Expression]Expression)
		for keyPair, keyValue := range node.Pairs {
			newKey, _ := Modify(keyPair, modifier).(Expression)
			newValue, _ := Modify(keyValue, modifier).(Expression)
			newPairs[newKey] = newValue
		}
		node.Pairs = newPairs
	}
	// If we’d only call modifier(node) and then return node, we wouldn’t be able to replace nodes in the AST, but only mutate them.
	// This line stops the recursion. Once we're here, we don't have any more traversable children and we can return
	return modifier(node)
}
