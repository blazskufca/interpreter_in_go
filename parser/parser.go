package parser

import (
	"fmt"
	"github.com/blazskufc/interpreter_in_go/ast"
	"github.com/blazskufc/interpreter_in_go/lexer"
	"github.com/blazskufc/interpreter_in_go/token"
	"strconv"
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

// These constants define the precedence/order of operations.
// The actual iota value does not matter, however the ORDERING does!!!
const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X or !X
	CALL        // myFunction(X)
)

// This is the precedence table for the parser.
// It associates the token types with their precedence.
var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
}

// Main idea in Pratt parser is the association of parsing functions with token types.
// Each token can have two functions associated with it, prefix and infix, depending on where the token is located.
// This is the definition of these two parsing functions.
type (
	prefixParseFn func() ast.Expression
	// Only the infixParseFn function takes an argument (prefixParseFn does not) which is an ast.Expression.
	// This argument represents the left side of the infix operator (a prefix operator and therefore prefixParseFn does not have a "left" side per definition)
	infixParseFn func(ast.Expression) ast.Expression
)

type Parser struct {
	lex            *lexer.Lexer                      // lex holds a pointer to lexer.Lexer
	curToken       token.Token                       // curToken is the current token.Token
	peekToken      token.Token                       // peekToken is the next token.Token
	errors         []string                          // errors is a slice of errors (of type string!) encountered during parsing
	prefixParseFns map[token.TokenType]prefixParseFn // prefixParseFns is an associative map between token.TokenType and a prefix parsing function (prefixParseFn)
	infixParseFns  map[token.TokenType]infixParseFn  // infixParseFns is an associative map between token.TokenType and a infix parsing function (infixParseFn)
}

// NewParser accepts a pointer to lexer.Lexer and returns a new pointer to Parser after initializing it
// (assures Parser.curToken and Parser.peekToken are set).
func NewParser(lex *lexer.Lexer) *Parser {
	p := &Parser{lex: lex, errors: []string{}, prefixParseFns: make(map[token.TokenType]prefixParseFn), infixParseFns: make(map[token.TokenType]infixParseFn)}
	// Registering prefixParseFn for tokens
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	// Registering infixParseFn for tokens
	// Note that every infix operator gets associated with the same function in this case
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	// Read two tokens, so Parser.curToken and Parser.peekToken are both set
	for i := 0; i < 2; i++ {
		p.nextToken()
	}
	return p
}

// parseIdentifier is an associated prefixParseFn for the token.Identifier type.
// It only returns a *ast.Identifier with the current token in the ast.Identifier.Token field and the literal value of
// the token in ast.Identifier.Value
// It doesn’t advance the tokens, it doesn’t call nextToken.
func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
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
		// Since the only two real statements in Monkey lang are "let"s and "returns", we try to parse an expression
		// token type is neither of those two!
		return p.parseExpressionStatement()
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

// registerPrefix adds an association to Parser.prefixParseFns map for the specified token.TokenType
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// registerInfix adds an association to Parser.infixParseFns map for the specified token.TokenType
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// parseExpressionStatement tries to parse Parser.curToken as na ast.ExpressionStatement.
// It returns a pointer to ast.ExpressionStatement.
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	statement := &ast.ExpressionStatement{Token: p.curToken}
	statement.Expression = p.parseExpression(LOWEST)
	// If the next token is a token.SEMICOLON (;) we advance the Parser pointers so that token.SEMICOLON is the parsers
	// curToken.
	// If the next token is not a SEMICOLON, that's OK too! We don't add an error to the parser
	// We want expressions to have optional semicolons.
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return statement
}

// parseExpression takes a precedence/order of operation value, as defined in parser constants.
// It then tries to find an prefixParseFn associated with Parser.curToken in Parser.prefixParseFns map.
// If there is no association (Parser.prefixParseFns[ Parser.curToken ] == nil) a nil is returned.
// Otherwise a value produced by calling the associated prefixParseFn is returned (which is an ast.Expression)
// The algorithm behind the parseExpression method and its combination of parsing functions and precedences is fully
// described by Vaughan Pratt in his “Top Down Operator Precedence” paper, however there are differences between his specification
// and this implementation
// Pratt doesn’t use a Parser structure and doesn’t pass around methods defined on *Parser. He also doesn’t use maps.
// Also what we call prefixParseFns are “nuds” (for “null denotations”) for Pratt. infixParseFns are “leds” (for “left denotations”).
/*
Suppose we’re parsing the following expression statement:
										1 + 2 + 3;

What we want is an AST that (serialized as a string) looks like this:
										((1 + 2) + 3)
The AST needs to have two *ast.InfixExpression nodes.
The *ast.InfixExpression higher in the tree should have the integer literal 3 as its ast.InfixExpression.Right
child node and its ast.InfixExpression.Left child node needs to be the other *ast.InfixExpression.

This second *ast.InfixExpression then needs to have the integer literals 1 and 2 as its ast.InfixExpression.Left and
ast.InfixExpression.Right child nodes, respectively. Like this:

											*ast.InfixExpression
							                                |                 \
											|		   \
											|		    \
											|		     \
											|		      \
							                                |                      \
							                                |                       \
											|			 \
								                        |                         \
											|			   \
											|			    \
											|			     \
							                         *ast.InfixExpression     	*ast.IntegerLiteral
							                            /           \               	|
							                 *ast.IntegerLiteral *ast.IntegerLiteral  	3
							                           |                |
							                           1                2

Here is what happens when we parse 1 + 2 + 3;:

1. parseExpressionStatement calls parseExpression(LOWEST) - The Parser.curToken and Parser.peekToken are the 1 and the first +:

										1    +    2    +    3    ;
										^    ^
										|    |
										|    Parser.peekToken
										|
										Parser.curToken

2. The first thing parseExpression then does is to check whether there is a prefixParseFn associated with the current Parser.curToken.Type, which is a token.INT.
	1. And, yes, there is: parseIntegerLiteral

3. So it calls parseIntegerLiteral, which returns an *ast.IntegerLiteral.

4. parseExpression assigns this to leftExp

5. Then comes the for-loop in parseExpression
	1. Its condition evaluates to true
		1. Parser.peekToken is not token.SEMICOLON
		2. peekPrecedence is higher than the argument passed to parseExpression, which is LOWEST. (see precedences map)
	2. So the loop is executed
		1. It fetches the infixParseFn for Parser.peekToken, which is parseInfixExpression
		2. The output of the parseInfixExpression is re-assigned to leftExp
		3.  it advances the tokens so they now look like this:

									1    +     2  +     3     ;
									     ^     ^
									     |     |
									     |     Parser.peekToken
									     |
									Parser.curToken

		4. With the tokens in this state, it calls parseInfixExpression and passes in the already parsed *ast.IntegerLiteral assigned to the leftExp outside the for loop
		5. It’s important to note that left in parseInfixExpression is our already parsed *ast.IntegerLiteral that represents the 1.
		6. parseInfixExpression saves the precedence of Parser.curToken (the first + token!)
		7. It advances the tokens by calling Parser.nextToken
		8. It calls parseExpression passing in the saved precedence from step 6
		9. So now parseExpression is called the second time, with the tokens looking like this:

										1     +     2     +     3     ;
											    ^     ^
											    |     |
											    |     Parser.peekToken
											    |
										    Parser.curToken
		10. The first thing parseExpression does again is to look for a prefixParseFn for Parser.curToken (which is 2)
		11. This is again parseIntegerLiteral
		12. But now the condition of the for-loop DOES NOT evaluate to true:
			1. precedence (the argument passed to parseExpression) is the precedence of the first "+" operator in "1 + 2 + 3",
			which is not smaller than the precedence of Parser.peekToken, the second "+" operator.
			2. They are equal
		13. The body of the for-loop is not executed and the *ast.IntegerLiteral representing the 2 is returned.
		14. Now back in parseInfixExpression the return-value of parseExpression is assigned to the ast.InfixExpression.Right
			field of the newly constructed *ast.InfixExpression.
		15. This *ast.InfixExpression gets returned by parseInfixExpression

												 +------------------------+
												 | *ast.InfixExpression   |
												 +------------------------+
												          /        \
													 /          \
													/            \
										+----------------------+  +----------------------+
										| *ast.IntegerLiteral  |  | *ast.IntegerLiteral  |
										+----------------------+  +----------------------+
												|                        |
												|                        |
												v                        v
											       (1)                      (2)

6. Now we’re back in the outer-most call to parseExpression, where precedence is still LOWEST.
7. We are back where we started and the condition of the for-loop is evaluated again
8. It still evaluates to true since precedence is LOWEST and peekPrecedence now returns the precedence of the second +
in our expression, which is higher.
9. parseExpression executes the body of the for-loop a second time.
	1. The difference is that now leftExp is not an *ast.IntegerLiteral representing the 1, but the *ast.InfixExpression
		returned by parseInfixExpression, representing 1 + 2
	2. In the body of the loop parseExpression fetches parseInfixExpression as the infixParseFn for Parser.peekToken
		(which is the second +), advances the tokens by calling nextToken and calls parseInfixExpression with leftExp as
		the argument.
	3. parseInfixExpression in turn calls parseExpression again, which returns the last *ast.IntegerLiteral (that
		represents the 3 in our expression).
	4. After all this, at the end of the loop-body, leftExp looks like this which is exactly what we wanted.
		The operators and operands are nested correctly!


											*ast.InfixExpression
							                                |                 \
											|		   \
											|		    \
											|		     \
											|		      \
							                                |                      \
							                                |                       \
											|			 \
								                        |                         \
											|			   \
											|			    \
											|			     \
							                         *ast.InfixExpression     	*ast.IntegerLiteral
							                            /           \               	|
							                 *ast.IntegerLiteral *ast.IntegerLiteral  	3
							                           |                |
							                           1                2
				  

	5. And our tokens look like this:

										1     +     2     +     3     ;
													^     ^
													|     |
													|     Parser.peekToken
													|
												Parser.curToken
	6. The condition of the for-loop evaluates to false:
		1. Now Parser.peekTokenIs (token.SEMICOLON) evaluates to true, which stops the body of the loop from
			being executed again.

10. The for-loop is done and leftExp is returned.
11. We’re back in parseExpressionStatement and have the final and correct *ast.InfixExpression at hand.
12. It is used as  used as the ast.Expression in *ast.ExpressionStatement.

This explanation comes from https://interpreterbook.com/
If you're still unsure about what's being explained here, buy the book for deeper explanation (there is also a traced
parseExpression, which could help with understanding)
*/
func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		// If there is no prefix that's a parsing error! Log it to Parser.errors!
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()
	// While the next token is not ";" (end of an expression) and the passed in precedence is less then the next token precedence...
	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		// Tries to find the infixParseFn for Parser.peekToken if the above condition is true
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}
		p.nextToken()
		// Call it by passing in the value (ast.Expression) produced from the call in prefixParseFn on Parser.curToken
		leftExp = infix(leftExp)
	}
	return leftExp
}

// parseIntegerLiteral tries to construct a ast.IntegerLiteral from Parser.curToken.
// It does so by transforming the Parser.curToken literal value from a string to int64 which is then assigned to ast.IntegerLiteral.Value.
// If it fails to do this conversion a nil is returned! Otherwise, an ast.Expression is returned!
// parseIntegerLiteral is also an associated prefixParseFn for the token.INT type.
func (p *Parser) parseIntegerLiteral() ast.Expression {
	literal := &ast.IntegerLiteral{Token: p.curToken}
	// Since the ast.IntegerLiteral.Value is an int64 we have to transform the token literal value to an int64
	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil { // If we can't transform the token literal from string to int64 that's a parsing error, so add it to parser errors
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}
	literal.Value = value
	return literal
}

// noPrefixParseFnError is a helper function which adds an error to Parser.errors when there is no registered prefixParseFn
// for the function parameter "t" in Parser.prefixParseFns.
func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	message := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, message)
}

// parsePrefixExpression creates and returns an ast.PrefixExpression.
// It's also the associated function (prefixParseFn) on Parser.prefixParseFns for types token.BANG and token.MINUS, the
// two known prefixes in Monkey lang.
func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	// We actually advance the token here. If you think about it, it should make sense!
	// The Parser.curToken before advancing the Parser pointers is either "!" (token.BANG) or "-" (token.MINUS)
	// So we have the prefix, but we still have to get the "right" (ast.PrefixExpression.Right) part, the part after the prefix...
	// For example, in "-5" the next token is 5
	p.nextToken()
	// ParseExpression here check the registered parsePrefixFn for 5 (which is token.INT), so it constructs ast.IntegerLiteral node in this case as the ast.PrefixExpression.Right
	expression.Right = p.parseExpression(PREFIX)
	return expression
}

// peekPrecedence tries to find a Parser.peekToken precedence value associated with this token type in precedences table.
// It the value is found in the table it returns the found value, which is an int.
// If the value for that token is not found in the table, LOWEST is returned, which is the lowest value precedence value any token can have as the default value.
func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

// curPrecedence tries to find a Parser.curToken precedence value associated with this token type in precedences table.
// It the value is found in the table it returns the found value, which is an int.
// If the value for that token is not found in the table, LOWEST is returned, which is the lowest value precedence value any token can have as the default value.
// It is functionally identical to peekPrecedence except for the fact that it check Parser.curToken instead of Parser.peekToken.
func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

// parseInfixExpression creates an ast.InfixExpression and returns it.
// One notable things is the fact that it accepts "left" argument, which is an ast.Expression and assigns it to ast.InfixExpression.Left.
func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{Token: p.curToken, Left: left, Operator: p.curToken.Literal}
	precedence := p.curPrecedence()
	// We have to advance the tokens, because the current token is the operator token...
	// We still need the right part of the InfixExpression however...
	p.nextToken()
	expression.Right = p.parseExpression(precedence) // Note that we pass the precedence we've got in the previous token as precedence to parseExpression
	return expression
}
