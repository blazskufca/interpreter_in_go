package object

import (
	"bytes"
	"fmt"
	"github.com/blazskufc/interpreter_in_go/ast"
	"strings"
)

/*
There are many different ways to build an "object" system (which in this case does not refer to OOP, but to the
way values are represented internally when the AST is evaluated/compiled to byte code/JIT-ed (there are also many way to
do this...heh)

"Object" system could also be called "value system", "object representation" (and probably by more terms/ways) if that
makes more sense..

What matters is that we need a system which can represent the values we've build during parsing/construction of our AST.

Let's say we have this example in our Monkey Lang:

											let a = 5;
											...
											...
											...
											a + a;

We bind integer literal 5 to name "a".
Then things happen.
But then we need to get the value bound to name "a" when we're evaluating "a + a"

In our case it's easy to see that we need to get the 5, which is *ast.IntegerLiteral in our AST.

But we need to decide how to represent this when evaluating the rest of the AST.

One way to do this is using the native types of the host language i.e. not wrap them in anything.

Another way would be to represent them as pointer.

Or native types and pointers could be mixed.

What to choose mainly depends on the following four (+1) things:
	1. Your language specification - How do you want to represent lets say a string? And an integer? What about a compound type?
	2. You host language - You can't represent a string the same way in C as you would in python, go... So
		the language in which you choose to implement the interpret matters
	3. Performance - Boxing types are generally slower... Do you care? Or maybe you just care about your object system being
		""easy"" to understand, extend, modify...
	4. What's the API for your values? How do you want to expose them to users?
	5. Misc - You need to think about way more things. For example, are you also implementing your own garbage collector?
		Then you need a way to track/count references...So you might box the types maybe...A lot more things like this.

The best way to see these decisions/ideas is to read through the source code of interpreters/compilers like https://github.com/wren-lang/wren
*/

type ObjectType string

const (
	INTEGER_OBJ      = "INTEGER"
	BOOLEAN_OBJ      = "BOOLEAN"
	NULL_OBJ         = "NULL"
	RETURN_VALUE_OBJ = "RETURN_VALUE"
	ERROR_OBJ        = "ERROR"
	FUNCTION_OBJ     = "FUNCTION"
)

// Object is how any node in the AST is represented when evaluating the AST internally. Note that it's an interface!
// Actual values are their own structures, but they all fulfil the Object interface.
type Object interface {
	Type() ObjectType
	Inspect() string
}

// Integer is the internal Object representation of integers in Monkey language
type Integer struct {
	Value int64
}

// Inspect on Integer fulfils the Object.Inspect interface method.
// It returns the Integer.Value as an string.
func (i *Integer) Inspect() string { return fmt.Sprintf("%d", i.Value) }

// Type on Integer type fulfils the Object.Type interface method.
// It returns a constant, INTEGER_OBJ.
func (i *Integer) Type() ObjectType { return INTEGER_OBJ }

// Boolean is the internal Object representation of booleans in Monkey language.
// Boolean fulfils the Object interface.
type Boolean struct {
	Value bool
}

// Inspect on Boolean fulfils the Object.Inspect interface method.
// It returns the Boolean.Value as an string.
func (b *Boolean) Inspect() string { return fmt.Sprintf("%t", b.Value) }

// Type on Boolean type fulfils the Object.Type interface method.
// It returns a constant, BOOLEAN_OBJ.
func (b *Boolean) Type() ObjectType { return BOOLEAN_OBJ }

// Null represent a missing/null/nil/(whatever you want to call it) in monkey language.
// Null fulfils the Object interface.
type Null struct{}

// Type on Null type fulfils the Object.Type interface method.
// It returns a constant, NULL_OBJ
func (n *Null) Type() ObjectType { return NULL_OBJ }

// Inspect on Null fulfils the Object.Inspect interface method.
// Since Null does not have a value associated with it, Inspect on Null returns a constant string "null".
// Please note that this implementation of Object.Type differs from all the other types which fulfil the Object interface,
// not necessarily in terms of functionality to the user (it's a interface defined method), but in terms of implementation
// since there is no actual value bound to Null!
func (n *Null) Inspect() string { return "null" }

// ReturnValue wraps the Value it's supposed to return inside itself.
// This is done because we pass Monkey Language return statements through our evaluator, keep track of it and later on
// decide whether we should stop evaluating or not.
type ReturnValue struct {
	Value Object // Value is the value which was supposed to be returned in a Monkey return statement. It's a Object.
}

// Type on ObjectType type fulfils the Object.Type interface method.
// It returns a constant, RETURN_VALUE_OBJ
func (rv *ReturnValue) Type() ObjectType { return RETURN_VALUE_OBJ }

// Inspect on ReturnValue fulfils the Object.Inspect interface method.
// It returns the result of a call to ReturnValue.Value.Inspect.
func (rv *ReturnValue) Inspect() string { return rv.Value.Inspect() }

// Error represents an internal error in Monkey Language.
// Error is an error encountered during execution of a Monkey program. For example wrong operators, unsupported operations
// other internal errors and so on.
type Error struct {
	Message string // Message is the message for the encountered error.
}

// Type on Error type fulfils the Object.Type interface method.
// It returns a constant, ERROR_OBJ.
func (e *Error) Type() ObjectType { return ERROR_OBJ }

// Inspect on Error fulfils the Object.Inspect interface method.
// It returns the result a string -> "ERROR: Error.Message"
func (e *Error) Inspect() string { return "ERROR: " + e.Message }

// Function represent an Monkey function from the AST *ast.FunctionLiteral in the evaluator/internal object system.
// It's pretty similar to the ast.FunctionLiteral, except that it does NOT contain the Token filed (since we don't need it
// in the already parsed AST) and that it DOES contain the Env (object.Environment)!
/*
We need to change the Environment so that the references to parameters in the function’s body resolve to the correct arguments.
But we can’t just add these arguments to the current environment.
That could lead to previous bindings being overwritten, which is not what we want:

let i = 5;

let printNum = fn(i) {

	puts(i);

};

printNum(10);

puts(i);

If we were to overwrite the current environment before evaluating the body of printNum, the last line would also result
in 10 being printed.

What we need to do instead is to preserve previous bindings while at the same time making new ones available - we’ll call that “extending
the environment”.

Extending the environment means that we create a new instance of object.Environment with a pointer to the environment
it should extend.

By doing that we enclose a fresh and empty environment with an existing one.

When the new environment’s Function.Get method is called and it itself doesn’t have a value associated with the given
name, it calls the Get of the enclosing environment. That’s the environment it’s extending. And if that enclosing
environment can’t find the value, it calls its own enclosing environment and so on until there is no enclosing environment
anymore and we can safely say that we have an “ERROR: unknown identifier: foobar”.
*/
type Function struct {
	Parameters []*ast.Identifier   // Parameters are the parameters this Monkey function accepts
	Body       *ast.BlockStatement // Body is the body of this Monkey function
	Env        *Environment        // Env is the environment for this Monkey function. This allows for closures!
}

// Type on Function type fulfils the Object.Type interface method.
// It returns a constant, FUNCTION_OBJ.
func (f *Function) Type() ObjectType { return FUNCTION_OBJ }

// Inspect on Function fulfils the Object.Inspect interface method.
// It returns the result a stringified Function (Function.Parameters and Function.Body).
func (f *Function) Inspect() string {
	var out bytes.Buffer
	params := []string{}
	for _, p := range f.Parameters {
		params = append(params, p.String())
	}
	out.WriteString("fn")
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") {\n")
	out.WriteString(f.Body.String())
	out.WriteString("\n}")
	return out.String()
}
