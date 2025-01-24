package lexer

import "github.com/blazskufc/interpreter_in_go/token"

// TODO: Potential improvement: Support full unicode input streams instead of ASCII
// it’s worth pointing out that the Lexer only supports ASCII characters instead of the full Unicode range.
// In order to fully support Unicode and UTF-8 we would need to change Lexer.ch from a byte to rune and change the way
// we read the next characters, since they could be multiple bytes wide now.
// And then we’d also need to change a few other methods and functions we’ll see later on

// TODO: Potential improvement: Generalize functions like readIdentifier and readNumber, which are very similar in terms of functionality

// TODO: Abstract away the two character token generation into something called makeTwoCharToken or similar

type Lexer struct {
	input        string
	position     int  // position is the current position in input (points to current char/corresponds to ch byte)
	readPosition int  // readPosition current reading position in input (after current character = position + 1)
	ch           byte // ch is the current character under examination
}

// NewLexer return a new pointer to a Lexer
func NewLexer(input string) *Lexer {
	lexer := &Lexer{input: input}
	// Initialize the Lexer at the start by calling readChar
	lexer.readChar()
	return lexer
}

// readChar tries to give us the next char (assigns it to Lexer.ch) and advances Lexer.position and Lexer.readPosition
func (l *Lexer) readChar() {
	// Guard against out of bounds
	if l.readPosition >= len(l.input) {
		l.ch = 0 // Byte 0 in ASCII is "NUL"
	} else {
		// If not out of bounds pickup the current char to examine
		l.ch = l.input[l.readPosition]
	}
	// Advance the pointers
	l.position = l.readPosition
	l.readPosition += 1
}

// NextToken determines looks at Lexer.ch, the current byte under examination, and returns the appropriate token.Token
// It also advices the Lexer pointers by calling Lexer.readChar()
func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()

	// Switch over Lexer.ch, the byte/character we are currently examining
	switch l.ch {
	case '=':
		// If the next character is also a "=" then the actual token should be "==", token.EQ
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			tok = newToken(token.ASSIGN, l.ch)
		}
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case '-':
		tok = newToken(token.MINUS, l.ch)
	case '!':
		// If the next character is also a "=" then the actual token should be "!=", token.NOT_EQ
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			tok = newToken(token.BANG, l.ch)
		}
	case '/':
		tok = newToken(token.SLASH, l.ch)
	case '*':
		tok = newToken(token.ASTERISK, l.ch)
	case '<':
		tok = newToken(token.LT, l.ch)
	case '>':
		tok = newToken(token.GT, l.ch)
	case '"':
		tok.Type = token.STRING
		tok.Literal = l.readString()
	case '[':
		tok = newToken(token.LBRACKET, l.ch)
	case ']':
		tok = newToken(token.RBRACKET, l.ch)
	case 0:
		tok.Type = token.EOF
		tok.Literal = ""
	default:
		// If the byte is a ASCII letter we need to handle an identifier
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier() // read the whole identifier into token.Token literal field
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.ch) { // Integers
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			return tok
		} else { // The current byte is not a ASCII letter -> Not a valid identifier start
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}
	l.readChar()
	return tok
}

// readIdentifier method returns a read identifier as a string, as the name might suggest
// Note that is advances Lexer pointers while doing so...
func (l *Lexer) readIdentifier() string {
	position := l.position // Position of the current char under examination, Lexer.ch byte, save it locally, see below why
	for isLetter(l.ch) {   // While the byte is a letter continue advancing Lexer pointers
		l.readChar()
	}
	// Return a slice from position to Lexer.Position (characters which are letters) - This is our read identifier
	return l.input[position:l.position]

}

// skipWhitespace skips over any character which is considered to be a whitespace character by calling readChar for any
// such character.
// This is done because whitespace does not carry any significance in Monkey lang
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

// readNumber reads number from input stream and returns it.
// It's very similar to readIdentifier in terms of functionality.
func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

// peekChar is very similar to readChar except for the fact that it does NOT increment the Lexer.position and Lexer.readPosition.
// It only returns the next byte/char, that is the byte at Lexer.readPosition
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0 // Guard against out of bounds -> When it's out of bound return ASCII NUL byte
	} else { // If its not out of bounds, return next character, i.e Lexer.readPosition
		return l.input[l.readPosition]
	}
}

// newToken is a simple helper function. It accepts token.TokenType and ch Byte and returns a new token.Token
func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

// isLetter is a simple helper function. It accepts a byte and determines if the byte is a ASCII alphabet character
// Besides allowing ASCII alphabet letters, it also allows for '_' -> This is so identifiers can have underscores in them
// For example -> let monkey_lang = true;
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

// isDigit is a simple helper function. It checks if the current Lexer character is supposed to be a integer.
// Returns true if it is and false otherwise.
func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// readString reads characters between enclosing " as a string to create a token.STRING
func (l *Lexer) readString() string {
	// Save a Lexer.position into a local variable and advance it for 1 char forward
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position] // From previously saved position until the current lexer position (i.e. when we went into the break)
}
