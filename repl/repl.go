package repl

import (
	"bufio"
	"fmt"
	"github.com/blazskufc/interpreter_in_go/lexer"
	"github.com/blazskufc/interpreter_in_go/token"
	"io"
	"log"
)

const PROMPT = ">> "

// Start creates a new bufio.Scanner which reads from "in" io.Reader, creates a new lexer.Lexer which tokenizes the read
// input and prints out the tokens to "out" io.Writer.
func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	for {
		_, err := fmt.Fprintf(out, PROMPT)
		if err != nil {
			log.Printf("failed to read write to output: %v", err)
			continue
		}
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.NewLexer(line)

		for tok := l.NextToken(); tok.Type != token.EOF; tok = l.NextToken() {
			_, err = fmt.Fprintf(out, "%+v\n", tok)
			if err != nil {
				log.Printf("failed to read write to output: %v", err)
				continue
			}
		}
	}
}
