package object

import "fmt"

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
	INTEGER_OBJ = "INTEGER"
	BOOLEAN_OBJ = "BOOLEAN"
	NULL_OBJ    = "NULL"
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
