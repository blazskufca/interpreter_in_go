package repl

import (
	"bufio"
	"fmt"
	"github.com/blazskufc/interpreter_in_go/evaluator"
	"github.com/blazskufc/interpreter_in_go/lexer"
	"github.com/blazskufc/interpreter_in_go/object"
	"github.com/blazskufc/interpreter_in_go/parser"
	"io"
	"log"
)

const MONKEY_FACE = `
			__,__
   .--. .-"     "-. .--.
  / .. \\/  .-. .-.  \\/ .. \\
 | |  '|  /   Y   \\  |'  | |
 | \\  \\ \\  0   |   0  / / / |
  \\ '-, \\.-""""""""-./,.-' /
   ''-'  /_ ^   ^ _\\  '-''
        |  \\._ _./  |
         \\  \\ '~' /  /
          '._ '-=-' _.'
             '-----'
`

const PROMPT = ">> "

// Start creates a new bufio.Scanner which reads from "in" io.Reader, creates a new lexer.Lexer which tokenizes the read
// input and prints out the tokens to "out" io.Writer.
func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	env := object.NewEnvironment()
	// This is an environment for the macro system
	macroEnv := object.NewEnvironment()
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
		p := parser.NewParser(l)

		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			printParserErrors(out, p.Errors())
			continue
		}
		// This expands the macros
		evaluator.DefineMacros(program, macroEnv)
		expanded := evaluator.ExpandMacros(program, macroEnv)
		evaluated := evaluator.Eval(expanded, env)
		if evaluated != nil && evaluated.Type() == object.ERROR_OBJ {
			_, err = io.WriteString(out, evaluated.Inspect())
			if err != nil {
				log.Printf("failed to write to output: %v", err)
				continue
			}
			_, err = io.WriteString(out, "\n")
			if err != nil {
				log.Printf("failed to write to output: %v", err)
				continue
			}
		}
		//The old parse printout - > Print's the AST structure
		//_, err = io.WriteString(out, program.String())
		//if err != nil {
		//	log.Printf("failed to write to output: %v", err)
		//}
		//_, err = io.WriteString(out, "\n")
		//if err != nil {
		//	log.Printf("failed to write to output: %v", err)
		//}
	}
}

func printParserErrors(out io.Writer, errors []string) {
	_, err := io.WriteString(out, MONKEY_FACE)
	if err != nil {
		log.Printf("failed to write to output: %v", err)
		return
	}
	_, err = io.WriteString(out, "Woops! We ran into some monkey business here!\n")
	if err != nil {
		log.Printf("failed to write to output: %v", err)
		return
	}
	_, err = io.WriteString(out, " parser errors:\n")
	if err != nil {
		log.Printf("failed to write to output: %v", err)
		return
	}
	for _, msg := range errors {
		_, err = io.WriteString(out, "\t"+msg+"\n")
		if err != nil {
			log.Printf("failed to write to output: %v", err)
			continue
		}
	}
}
