package evaluator

import (
	"github.com/blazskufca/interpreter_in_go/object"
)

// This file contains Monkey builtin functions

// builtins defines a map literal of string to pointers of *object.Builtin.
var builtins = map[string]*object.Builtin{
	// len evaluates the length of strings in Monkey
	"len":     object.GetBuiltinByName("len"),
	"now":     object.GetBuiltinByName("now"),
	"type_of": object.GetBuiltinByName("type_of"),
	"size_of": object.GetBuiltinByName("size_of"),
	"first":   object.GetBuiltinByName("first"),
	"last":    object.GetBuiltinByName("last"),
	"rest":    object.GetBuiltinByName("rest"),
	"push":    object.GetBuiltinByName("push"),
	// This is terrible design, going down the Javascript route here... p treats arrays as immutable, but pop does not...
	// Good thing it's but toy language
	"pop":  object.GetBuiltinByName("pop"),
	"puts": object.GetBuiltinByName("puts"),
}
