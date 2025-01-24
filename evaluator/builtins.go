package evaluator

import (
	"github.com/blazskufc/interpreter_in_go/object"
	"time"
)

// This file contains Monkey builtin functions

// builtins defines a map literal of string to pointers of *object.Builtin.
var builtins = map[string]*object.Builtin{
	// len evaluates the length of strings in Monkey
	"len": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1",
					len(args))
			}
			switch arg := args[0].(type) {
			case *object.String:
				return &object.Integer{Value: int64(len(arg.Value))}
			default:
				return newError("argument to `len` not supported, got %s",
					args[0].Type())
			}
		},
	},
	"now": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) == 1 {
				if formatArgument := args[0]; formatArgument.Type() == object.STRING_OBJ {
					stringObject, ok := formatArgument.(*object.String)
					if !ok {
						return newError("argument to `now` not supported, got %s", formatArgument.Type())
					}
					return &object.String{Value: time.Now().Format(stringObject.Value)}
				}
			}
			return &object.Integer{Value: int64(time.Now().Unix())} // avoid the 19.1.2038 on 32bit - Not sure if this is enough or even needed, probably not.
		},
	},
	"type_of": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1", len(args))
			}
			var gotType string
			switch args[0].(type) {
			case *object.Integer:
				gotType = object.INTEGER_OBJ
			case *object.Boolean:
				gotType = object.BOOLEAN_OBJ
			case *object.Null:
				gotType = object.NULL_OBJ
			case *object.ReturnValue:
				gotType = object.RETURN_VALUE_OBJ
			case *object.Error:
				gotType = object.ERROR_OBJ
			case *object.Function:
				gotType = object.FUNCTION_OBJ
			case *object.String:
				gotType = object.STRING_OBJ
			case *object.Builtin:
				gotType = object.BUILTIN_OBJ
			default:
				return newError("UNKNOWN TYPE %s", args[0].Type())
			}
			return &object.String{Value: gotType}
		},
	},
}
