package parser

import (
	"fmt"
	"github.com/blazskufc/interpreter_in_go/ast"
	"github.com/blazskufc/interpreter_in_go/lexer"
	"github.com/blazskufc/interpreter_in_go/token"
)

/*
This is a pseudocode example of recursive descent parser/Pratt parser; what this parser is trying to accomplish!

Reading and understanding what this pseudocode is trying to convey will make understanding the actual implementation easier.

function parseProgram() {
    program = newProgramASTNode()
    advanceTokens()

    while (currentToken() != EOF_TOKEN) {
        statement = null

        if (currentToken() == LET_TOKEN) {
            statement = parseLetStatement()
        } else if (currentToken() == RETURN_TOKEN) {
            statement = parseReturnStatement()
        } else if (currentToken() == IF_TOKEN) {
            statement = parseIfStatement()
        }

        if (statement != null) {
            program.Statements.push(statement)
        }
        advanceTokens()
    }
    return program
}

function parseLetStatement() {
    advanceTokens()
    identifier = parseIdentifier()
    advanceTokens()

    if currentToken() != EQUAL_TOKEN {
        parseError("no equal sign!")
        return null
    }

    advanceTokens()
    value = parseExpression()

    variableStatement = newVariableStatementASTNode()
    variableStatement.identifier = identifier
    variableStatement.value = value

    return variableStatement
}

function parseIdentifier() {
    identifier = newIdentifierASTNode()
    identifier.token = currentToken()
    return identifier
}

function parseExpression() {
    if (currentToken() == INTEGER_TOKEN) {
        if (nextToken() == PLUS_TOKEN) {
            return parseOperatorExpression()
        } else if (nextToken() == SEMICOLON_TOKEN) {
            return parseIntegerLiteral()
        }
    } else if (currentToken() == LEFT_PAREN) {
        return parseGroupedExpression()
    }
    // [...]
}

function parseOperatorExpression() {
    operatorExpression = newOperatorExpression()
    operatorExpression.left = parseIntegerLiteral()
    advanceTokens()
    operatorExpression.operator = currentToken()
    advanceTokens()
    operatorExpression.right = parseExpression()
    return operatorExpression
}
// [...]
*/

/*

Here is some terminology related to Pratt parser which you should probably understand:

- Prefix operator: A prefix operator is an operator “in front of” its operand
	- Example: --5
	- Here the operator is -- (decrement), the operand is the integer literal 5 and the operator is in
	the prefix position.


- A postfix operator is an operator “after” its operand.
	- Example: foobar++
	- Here the operator is ++ (increment), the operand is the identifier foobar and the operator isin the postfix position.
	- The Monkey interpreter we’ll build won’t have postfix operators.


- Infix operators are something we’ve all seen before. An infix operator sits between its operands.
	- Example: 5 * 8
	- The * operator sits in the infix position between the two integer literals 5 and 8.
	- Infix operators appear in binary expressions - where the operator has two operands.


- Operator precedence/order of operations - which priority do different operators have.
	- The canonical example is this one: 5 + 5 * 10
		- The result of this expression is 55 and NOT 100.
	- That’s because the * operator has a higher precedence, a “higher rank”.
	- It’s “more important” than the + operator. It gets evaluated before the other operator.
*/

type Parser struct {
	lex       *lexer.Lexer // lex holds a pointer to lexer.Lexer
	curToken  token.Token  // curToken is the current token.Token
	peekToken token.Token  // peekToken is the next token.Token
	errors    []string     // errors is a slice of errors (of type string!) encountered during parsing
}

// NewParser accepts a pointer to lexer.Lexer and returns a new pointer to Parser after initializing it
// (assures Parser.curToken and Parser.peekToken are set).
func NewParser(lex *lexer.Lexer) *Parser {
	p := &Parser{lex: lex, errors: []string{}}
	// Read two tokens, so Parser.curToken and Parser.peekToken are both set
	for i := 0; i < 2; i++ {
		p.nextToken()
	}
	return p
}

// nextToken sets the Parser.curToken and Parser.peekToken to their respective values (Parser.peekToken is one ahead of Parser.curToken)
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.lex.NextToken()
}

// ParseProgram is the "main loop" of the parser.
// It initializes the root node, i.e. ast.Program structure, parses child nodes and then returns a pointer to the root node.
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}              // Construct new root node
	program.Statements = []ast.Statement{} // Initialize the Statements slice
	for !p.curTokenIs(token.EOF) {         // while the current token is not EOF token, i.e. end of input file/program
		statement := p.parseStatement()
		if statement != nil {
			program.Statements = append(program.Statements, statement)
		}
		p.nextToken() // Advance token until we get all EOF token, i.e. we've parsed the requested program
	}
	return program // Return the parsed program
}

// parseStatement looks at the current token, Parser.curToken, decides on how it should be parsed and then returns the
// parsed ast.Statement.
// If the parseStatement does not know how to parse the current token nil is returned!
func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return nil
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	statement := &ast.LetStatement{Token: p.curToken} // p.curToken should be token.LET
	// "let" should be followed by binding name identifier.
	// If it's not, return nil
	if !p.expectPeek(token.IDENT) {
		return nil
	}

	statement.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// Here we must get "="
	// As a reminder, monkey source code looks like this "let x = 5;"
	// We've parsed the "let x" so assignment MUST follow!
	if !p.expectPeek(token.ASSIGN) {
		return nil
	}
	// TODO: We're skipping the expressions until we
	// encounter a semicolon
	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return statement
}

// curTokenIs is a helper method. It checks weather Parser.curToken is of the same type as the provided parameter.
// Returns true if it is and false otherwise.
func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

// peekTokenIs is a helper method. It checks weather Parser.peekToken is of the same type as the provided parameter t.
// Logically it's almost identical to curTokenIs method on Parser type, except that peekTokenIs checks the Parser.peekToken
// while curTokenIs check Parser.curToken.
// Return true if provided token and Parser.peekToken are the same, false otherwise.
func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

// expectPeek check weather the provided parameter t is the same as Parser.peekToken by calling peekTokenIs.
// If they are the same, Parser pointers (Parser.curToken and Parser.peekToken) are advanced via a call to nextToken then
// true is returned.
// If parameter t and Parser.peekToken are NOT the same, false is returned and Parser pointers (Parser.curToken and Parser.peekToken)
// are NOT advanced!
func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

// peekError is a Parser helper method. It adds an error into Parser.errors if Parser.peekToken is not the expected token.
func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead",
		t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

// Errors Return slice Parser.errors
func (p *Parser) Errors() []string {
	return p.errors
}

// parseReturnStatement parses ast.ReturnStatement statements.
// It returns a pointer to ast.ReturnStatement
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	statement := &ast.ReturnStatement{Token: p.curToken} // p.curToken here should be token.RETURN
	p.nextToken()                                        // get the <expression>
	// TODO: We're skipping the expressions until we
	// encounter a semicolon
	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return statement
}
