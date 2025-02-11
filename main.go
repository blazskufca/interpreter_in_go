package main

import (
	"fmt"
	"github.com/blazskufc/interpreter_in_go/repl"
	"log"
	"os"
	"os/user"
)

// main start a repl with repl.Start and sets os.Stdin as the "in" and os.Stdout as the "out" arguments
func main() {

	currUser, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Hello %s! This is the Monkey programming language!\n",
		currUser.Username)
	fmt.Printf("Feel free to type in commands\n")
	repl.Start(os.Stdin, os.Stdout)
}
