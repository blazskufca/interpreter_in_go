package main

import (
	"flag"
	"fmt"
	"github.com/blazskufca/interpreter_in_go/repl"
	"log"
	"os"
	"os/user"
)

var MODE string

// main start a repl with repl.Start and sets os.Stdin as the "in" and os.Stdout as the "out" arguments
func main() {
	flag.StringVar(&MODE, "m", "inplace-evaluator", "Specify which backend is used. inplace-evaluator or bytecode")
	flag.Parse()
	currUser, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Hello %s! This is the Monkey programming language!\nEvaluator mode: %s\n",
		currUser.Username, MODE)
	fmt.Printf("Feel free to type in commands\n")
	repl.Start(os.Stdin, os.Stdout, MODE)
}
