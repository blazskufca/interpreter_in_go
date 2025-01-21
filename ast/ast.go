package ast

import (
	"bytes"
	"github.com/blazskufc/interpreter_in_go/token"
)

/*
As an example and a demonstration of the general idea, the following monkey lang code:
									  let x = 5;

Could be represented in the AST as:
						            +------------+
						            |*ast.Program|
						            |------------|
						            | Statements |
						            +-----+------+
						                  |
						                  v
						            +------------+
						            |*ast.LetStmt|
						            |------------|
						            |    Name    |
						            |    Value   |
						            +-----+------+
						                  |
						        +---------+---------+
						        |                   |
						        v                   v
						+--------------+    +---------------+
						|*ast.Identifer|    |*ast.Expression|
						+--------------+    +---------------+
*/

// Node is the most basic type of a tree node in our AST.
// Each subtype of a node must implement the Node interface.
type Node interface {
	TokenLiteral() string // TokenLiteral returns the literal value associated with the token. Will be used for only debugging and testing.
	String() string
}

// Statement is a AST Node which represents a Statement in Monkey programming language, as the name might suggest
type Statement interface {
	Node
	statementNode()
}

// Expression is a AST Node which represents an Expression in Monkey programming language, as the name might suggest
type Expression interface {
	Node
	expressionNode()
}

// The Program node is the root node of every AST the Monkey programing language produces!
type Program struct {
	Statements []Statement // Every valid Program is a series of Statements
}

// Identifier holds the identifier of the binding. It implements the Expression interface
type Identifier struct {
	Token token.Token // the token.IDENT token
	Value string
}

type LetStatement struct {
	Token token.Token // Token is a token.Token which produces this "let" expression.
	Name  *Identifier // Identifier associated with the expression. Name holds the identifier of the binding.
	Value Expression  // Value is an Expression associated with this Name. Value holds the Expression that produces the value.
}

func (p *Program) TokenLiteral() string {
	// Guard against no present statements in the program
	if len(p.Statements) > 0 {
		// If there are statements return the TokenLiteral value of the first token in the program
		return p.Statements[0].TokenLiteral()
	} else {
		return "" // If there are no statements return an empty string
	}
}

// String on Program type satisfies the Node interface (and consequently the fmt.Stringer)
// It returns stringified contents of Program.Statements slice.
func (p *Program) String() string {
	var out bytes.Buffer
	for _, s := range p.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}

// statementNode satisfies the Statement interface on the LetStatement structure
func (ls *LetStatement) statementNode() {}

// TokenLiteral satisfies the Node interface on LetStatement structure.
// It returns the Token Literal of the Token associated with the LetStatement
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

// String on LetStatement type satisfies the Node interface (and consequently the fmt.Stringer)
// It returns stringified contents of LetStatement (LetStatement.Name, LetStatement.TokenLiteral and LetStatement.Value).
func (ls *LetStatement) String() string {
	var out bytes.Buffer
	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")
	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}
	out.WriteString(";")
	return out.String()
}

// expressionNode satisfies the Expression interface on the Identifier structure.
func (i *Identifier) expressionNode() {}

// TokenLiteral satisfies the Node interface on Identifier structure.
// It returns the Token Literal of the Token associated with the Identifier
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }

// String on Identifier type satisfies the Node interface (and consequently the fmt.Stringer)
// It returns stringified contents of Identifier (Identifier.Value).
func (i *Identifier) String() string { return i.Value }

type ReturnStatement struct {
	Token       token.Token // Token should be the token.RETURN token
	ReturnValue Expression  // ReturnValue Expression is the expression in "return <expression>;"
}

// statementNode satisfies the Statement interface on the ReturnStatement structure
func (rs *ReturnStatement) statementNode() {}

// TokenLiteral satisfies the Node interface on ReturnStatement structure.
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }

// String on ReturnStatement type satisfies the Node interface (and consequently the fmt.Stringer)
// It returns stringified contents of ReturnStatement (ReturnStatement.TokenLiteral and LetStatement.Value).
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer
	out.WriteString(rs.TokenLiteral() + " ")
	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}
	out.WriteString(";")
	return out.String()
}

/*
ExpressionStatement encompasses the following code structure in monkey lang:

	let x = 5;
	x + 10; // This line is an expression statement.
*/
type ExpressionStatement struct {
	Token      token.Token // Token here will be the first token of the expression
	Expression Expression  // Expression is the actual expression in "expression" statement
}

// statementNode satisfies the Statement interface on the ExpressionStatement structure
func (es *ExpressionStatement) statementNode() {}

// TokenLiteral satisfies the Node interface on ExpressionStatement structure.
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }

// String on ExpressionStatement type satisfies the Node interface (and consequently the fmt.Stringer)
// It returns stringified contents of ExpressionStatement (ExpressionStatement.String).
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

type IntegerLiteral struct {
	Token token.Token // Token is the token associated with this integer literal
	Value int64       // The Value of an integer literal is an int64, which should make sense, since they are integers...
}

// expressionNode on IntegerLiteral fulfills the Expression interface.
func (il *IntegerLiteral) expressionNode() {}

// TokenLiteral on IntegerLiteral fulfills the Node interface.
// It returns the literal value of IntegerLiteral.Token
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }

// String on IntegerLiteral type satisfies the Node interface (and consequently the fmt.Stringer)
// It returns stringified contents of IntegerLiteral (literal value of IntegerLiteral.Token).
func (il *IntegerLiteral) String() string { return il.Token.Literal }

type PrefixExpression struct {
	Token    token.Token // Token is the prefix token, e.g. "!"
	Operator string      // Operator is a string which contains the operator literal, e.g. "!" or "-"
	Right    Expression  // Right contains the Expression on the right side of the Operator
}

// expressionNode on type PrefixExpression fulfills the Expression interface.
func (pe *PrefixExpression) expressionNode() {}

// TokenLiteral on type PrefixExpression fulfills the Node interface.
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }

// String on PrefixExpression type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of PrefixExpression (PrefixExpression.Operator and PrefixExpression.Right).
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")
	return out.String()
}

type InfixExpression struct {
	Token    token.Token // Token is the operator token, e.g. +
	Left     Expression  // Left is the Expression on the left side of the Operator
	Operator string      // Operator is a string literal of the operator, e.g. "+", "-"
	Right    Expression  // Right is the Expression on the right side of the Operator
}

// expressionNode on type InfixExpression fulfills the Expression interface.
func (ie *InfixExpression) expressionNode() {}

// TokenLiteral on type InfixExpression fulfills the Node interface.
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }

// String on InfixExpression type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of InfixExpression (InfixExpression.Left, InfixExpression.Operator and InfixExpression.Operator).
func (ie *InfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")
	return out.String()
}

type Boolean struct {
	Token token.Token // Token is the token containing the boolean literal
	Value bool        // Value is a boolean here, either true or false
}

// expressionNode on type Boolean satisfies the Expression interface
func (b *Boolean) expressionNode() {}

// TokenLiteral on type Boolean satisfies the Node interface.
// It returns the literal value of  Boolean.Token.
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }

// String on Boolean type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of Boolean (Boolean.Token).
func (b *Boolean) String() string { return b.Token.Literal }

// IfExpression in Monkey Lang follows the following structure -> if (<condition>) <consequence> else <alternative>
type IfExpression struct {
	Token       token.Token     // Token is the "if" token
	Condition   Expression      // Condition is the Expression which kicks off the if body
	Consequence *BlockStatement // Consequence is the *BlockStatement which is evaluated if the if Condition is true
	Alternative *BlockStatement // Alternative is the *BlockStatement which is evaluated if the Condition is false
}

// expressionNode on type IfExpression satisfies the Expression interface
func (ie *IfExpression) expressionNode() {}

// TokenLiteral on type IfExpression satisfies the Node interface.
// It returns the literal of IfExpression.Token.
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }

// String on IfExpression type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of IfExpression (IfExpression.Condition, IfExpression.Consequence and IfExpression.Alternative).
func (ie *IfExpression) String() string {
	var out bytes.Buffer
	out.WriteString("if")
	out.WriteString(ie.Condition.String())
	out.WriteString(" ")
	out.WriteString(ie.Consequence.String())
	if ie.Alternative != nil {
		out.WriteString("else ")
		out.WriteString(ie.Alternative.String())
	}
	return out.String()
}

type BlockStatement struct {
	Token      token.Token // Token is the "{" token
	Statements []Statement // Statements in a slice of Statement -> Statements contained inside the {...}
}

// statementNode on type BlockStatement satisfies the Statement interface
func (bs *BlockStatement) statementNode() {}

// TokenLiteral on type BlockStatement satisfies the Node interface.
// It returns the literal value of BlockStatement.Token.
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }

// String on BlockStatement type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of BlockStatement (Statements inside BlockStatement.Statements are stringified and returned).
func (bs *BlockStatement) String() string {
	var out bytes.Buffer
	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}
