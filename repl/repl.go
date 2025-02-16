package repl

import (
	"bufio"
	"fmt"
	"github.com/blazskufca/interpreter_in_go/compiler"
	"github.com/blazskufca/interpreter_in_go/evaluator"
	"github.com/blazskufca/interpreter_in_go/lexer"
	"github.com/blazskufca/interpreter_in_go/object"
	"github.com/blazskufca/interpreter_in_go/parser"
	"github.com/blazskufca/interpreter_in_go/vm"
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

var ENV, MACRO_ENV *object.Environment
var CONSTANTS []object.Object
var GLOBALS = make([]object.Object, vm.GlobalsSize)
var SYMBOL_TABLE = compiler.NewSymbolTable()

// Start creates a new bufio.Scanner which reads from "in" io.Reader, creates a new lexer.Lexer which tokenizes the read
// input and prints out the tokens to "out" io.Writer.
func Start(in io.Reader, out io.Writer, MODE string) {
	scanner := bufio.NewScanner(in)
	if MODE == "inplace-evaluator" {
		ENV = object.NewEnvironment()
		// This is an environment for the macro system
		MACRO_ENV = object.NewEnvironment()
	} else if MODE == "bytecode" {
		for i, v := range object.Builtins {
			SYMBOL_TABLE.DefineBuiltin(i, v.Name)
		}
	}
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
		if MODE == "inplace-evaluator" {
			if ENV == nil {
				log.Fatal("the ENV of type *object.Environment is nil when it shouldn't be...this is a fatal error!")
			}
			if MACRO_ENV == nil {
				log.Fatal("the MACRO_ENV of type *object.Environment is nil when it shouldn't be...this is a fatal error!")
			}
			// This expands the macros
			evaluator.DefineMacros(program, MACRO_ENV)
			expanded := evaluator.ExpandMacros(program, MACRO_ENV)
			evaluated := evaluator.Eval(expanded, ENV)
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
		} else if MODE == "bytecode" {
			comp := compiler.NewWithState(SYMBOL_TABLE, CONSTANTS)
			err := comp.Compile(program)
			if err != nil {
				_, err = fmt.Fprintf(out, "Woops! Compilation failed:\n %s\n", err)
				if err != nil {
					log.Printf("failed to write to output: %v", err)
				}
				continue
			}
			code := comp.Bytecode()
			CONSTANTS = code.Constants
			machine := vm.NewWithGlobalsStore(code, GLOBALS)
			err = machine.Run()
			if err != nil {
				_, err = fmt.Fprintf(out, "Woops! Executing bytecode failed:\n %s\n", err)
				if err != nil {
					log.Printf("failed to write to output: %v", err)
				}
				continue
			}
			stackTop := machine.LastPoppedStackElem()
			if stackTop == nil && stackTop.Type() == object.ERROR_OBJ {
				_, err = io.WriteString(out, stackTop.Inspect())
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
