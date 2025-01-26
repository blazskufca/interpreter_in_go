package ast

import (
	"bytes"
	"github.com/blazskufc/interpreter_in_go/token"
	"strings"
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

type FunctionLiteral struct {
	Token      token.Token     // Token is the "fn" token
	Parameters []*Identifier   // Parameters are the function parameters in Monkey lang -> fn <parameters <<parameter one>, <parameter two>, <parameter three>, ...> > <block statement>
	Body       *BlockStatement // Body is the body of the function in Monkey Lang
}

// expressionNode on FunctionLiteral fulfills the Expression interface.
func (fl *FunctionLiteral) expressionNode() {}

// TokenLiteral on FunctionLiteral fulfills the Node interface.
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Literal }

// String on FunctionLiteral type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of FunctionLiteral (FunctionLiteral.Parameters and FunctionLiteral.Body).
func (fl *FunctionLiteral) String() string {
	var out bytes.Buffer
	params := []string{}
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}
	out.WriteString(fl.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(fl.Body.String())
	return out.String()
}

// CallExpression is an expression which is evaluated/constructed in the AST when a function call is encountered during
// parsing.
// CallExpression in monkey lang follow this specification:
//
// <expression>(<comma separated expressions>)
//
// Which means all of the following example are valid CallExpression's in monkey lang:
//
// add(2, 3)
//
// add(2 + 2, 3 * 3 * 3)
//
// callsFunction(2, 3, fn(x, y) { x + y; });
type CallExpression struct {
	Token     token.Token  // Token is the "(" token
	Function  Expression   // Function is the Identifier or FunctionLiteral
	Arguments []Expression // Arguments are function arguments supplied to FunctionLiteral / CallExpression
}

// expressionNode on CallExpression type satisfies the Expression interface.
func (ce *CallExpression) expressionNode() {}

// TokenLiteral on CallExpression fulfills the Node interface.
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }

// String on CallExpression type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of CallExpression (CallExpression.Arguments and CallExpression.Function).
func (ce *CallExpression) String() string {
	var out bytes.Buffer
	args := []string{}
	for _, a := range ce.Arguments {
		args = append(args, a.String())
	}
	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}

type StringLiteral struct {
	Token token.Token // Token is the token.STRING
	Value string      // Value is the string literal
}

// expressionNode on StringLiteral fulfills the Expression interface.
func (sl *StringLiteral) expressionNode() {}

// TokenLiteral on StringLiteral fulfills the Node interface.
// It returns the literal value of the StringLiteral.Token.
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }

// String on StringLiteral type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of StringLiteral (StringLiteral.Token.Literal).
func (sl *StringLiteral) String() string { return sl.Token.Literal }

// ArrayLiteral represents Monkey Language arrays.
// ArrayLiteral has a Elements slice of type Expression which represents the array elements.
type ArrayLiteral struct {
	Token    token.Token  // Token is the [ token
	Elements []Expression // Elements are the actual elements in the array. Type does not matter, any Expression is valid!
}

// expressionNode on ArrayLiteral fulfills the Expression interface.
func (al *ArrayLiteral) expressionNode() {}

// TokenLiteral on ArrayLiteral fulfills the Node interface.
// It returns the literal value of the ArrayLiteral.Token.
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Literal }

// String on ArrayLiteral type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of ArrayLiteral (all the array elements, ArrayLiteral.Elements).
func (al *ArrayLiteral) String() string {
	var out bytes.Buffer
	elements := []string{}
	for _, el := range al.Elements {
		elements = append(elements, el.String())
	}
	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")
	return out.String()
}

// IndexExpression is an index access operator, i.e.
/*
<expression>[<expression>]

or in Monkey source code:

myArray[0];

[1, 2, 3, 4][2];

let myArray = [1, 2, 3, 4];
myArray[2];

myArray[2 + 1];

returnsArray()[1];
*/
type IndexExpression struct {
	Token token.Token // The [ token
	Left  Expression  // Left is the object that’s being accessed.
	Index Expression  // Index is the access operator inside the brackets. Index has to produce an integer!
}

// expressionNode on IndexExpression fulfills the Expression interface.
func (ie *IndexExpression) expressionNode() {}

// TokenLiteral on IndexExpression fulfills the Node interface.
// It returns the literal value of the IndexExpression.Token.
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }

// String on IndexExpression type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of IndexExpression (IndexExpression.Left and IndexExpression.Index).
func (ie *IndexExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("])")
	return out.String()
}

// HashLiteral follow this structure: {<expression> : <expression>, <expression> : <expression>, ... };...
// Each pair consists of two expressions. One produces the hash key and one produces the value.
// The only admissible data types for hash keys are StringLiteral, IntegerLiteral and Boolean.
// But we can’t enforce that in the parser! Instead, we’ll have to validate hash key types in the evaluation stage and
// generate possible errors there.
/*
That’s because a lot of different expressions can produce strings, integers or booleans. Not just
their literal forms. Enforcing the data type of hash keys in the parsing stage would prevent us
from doing something like this:

let key = "name";

let hash = {key: "Monkey"};

Here key evaluates to "name" and is thus totally valid as a hash key, even though it’s an identifier.

In order to allow this, we need to allow any Expression as a key and any Expression as a value in a hash literal!

At least in the parsing stage.
*/
type HashLiteral struct {
	Token token.Token               // Token is the '{' token
	Pairs map[Expression]Expression // Pairs is a map[Expression]Expression...See HashLiteral type docs as to why!
}

// expressionNode on type HashLiteral fulfills the Expression interface.
func (hl *HashLiteral) expressionNode() {}

// TokenLiteral on type HashLiteral fulfils the Node interface.
// It returns the literal value of HashLiteral.Token.
func (hl *HashLiteral) TokenLiteral() string { return hl.Token.Literal }

// String on HashLiteral type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of HashLiteral (HashLiteral.Pairs is stingified).
func (hl *HashLiteral) String() string {
	var out bytes.Buffer
	pairs := []string{}
	for key, value := range hl.Pairs {
		pairs = append(pairs, key.String()+":"+value.String())
	}
	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")
	return out.String()
}

// MacroLiteral is a macro expression literal, i.e. macro(< Identifier, Identifier, Identifier ...>) < BlockStatement >.
// It's very similar to FunctionLiteral, deviates only in name.
type MacroLiteral struct {
	Token      token.Token     // Token is the "macro" token
	Parameters []*Identifier   // Parameters is a slice of *Identifier's, i.e. the function arguments the macro will receive
	Body       *BlockStatement // Body is the body of the macro, i.e. the actual code which the macro should execute
}

// expressionNode on type MacroLiteral fulfills the Expression interface.
func (ml *MacroLiteral) expressionNode() {}

// TokenLiteral on type MacroLiteral fulfils the Node interface.
// It returns the literal value of MacroLiteral.Token.
func (ml *MacroLiteral) TokenLiteral() string { return ml.Token.Literal }

// String on MacroLiteral type satisfies the Node interface (and consequently the fmt.Stringer).
// It returns stringified contents of MacroLiteral (MacroLiteral.Parameters, MacroLiteral.Token and MacroLiteral.Body).
func (ml *MacroLiteral) String() string {
	var out bytes.Buffer
	params := []string{}
	for _, p := range ml.Parameters {
		params = append(params, p.String())
	}
	out.WriteString(ml.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(ml.Body.String())
	return out.String()
}
