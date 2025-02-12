package evaluator

import (
	"fmt"
	"github.com/blazskufca/interpreter_in_go/object"
	"time"
	"unsafe"
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
			case *object.Array:
				return &object.Integer{Value: int64(len(arg.Elements))}
			default:
				return newError("argument to `len` not supported, got %s",
					args[0].Type())
			}
		},
	},
	"now": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			switch len(args) {
			case 0:
				return &object.Integer{Value: time.Now().Unix()}
			case 1:
				if formatArgument := args[0]; formatArgument.Type() == object.STRING_OBJ {
					stringObject, ok := formatArgument.(*object.String)
					if !ok {
						return newError("argument to `now` not supported, got %s", formatArgument.Type())
					}
					return &object.String{Value: time.Now().Format(stringObject.Value)}
				} else {
					return newError("wrong type of argument. got=%s, want=STRING", args[0].Type())
				}
			default:
				return newError("wrong number of arguments. got=%d, want=1 (format) or 0 for (unix epoch)", len(args))
			}
		},
	},
	"type_of": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1", len(args))
			}
			return &object.String{Value: string(args[0].Type())}
		},
	},
	"size_of": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1", len(args))
			}
			switch arg := args[0].(type) {
			case *object.Integer:
				return &object.Integer{Value: int64(unsafe.Sizeof(arg.Value))}
			case *object.Boolean:
				return &object.Integer{Value: int64(unsafe.Sizeof(arg.Value))}
			case *object.Null:
				return &object.Integer{Value: 0}
			case *object.ReturnValue:
				return &object.Integer{Value: int64(unsafe.Sizeof(arg.Value))}
			case *object.Error:
				return &object.Integer{Value: int64(unsafe.Sizeof(arg.Message))}
			case *object.Function:
				return &object.Integer{Value: int64(unsafe.Sizeof(arg.Body) + unsafe.Sizeof(arg.Parameters))}
			case *object.String:
				return &object.Integer{Value: int64(unsafe.Sizeof(arg.Value))}
			case *object.Builtin:
				return &object.Integer{Value: int64(unsafe.Sizeof(arg.Fn))}
			case *object.Array:
				return &object.Integer{Value: int64(unsafe.Sizeof(arg.Elements))}

			}
			return &object.Integer{Value: int64(unsafe.Sizeof(args[0]))}
		},
	},
	"first": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1",
					len(args))
			}
			if args[0].Type() != object.ARRAY_OBJ {
				return newError("argument to `first` must be ARRAY, got %s",
					args[0].Type())
			}
			arr, ok := args[0].(*object.Array)
			if !ok {
				return newError("argument to `first` must be ARRAY, got %s", args[0].Type())
			}
			if len(arr.Elements) > 0 {
				return arr.Elements[0]
			}
			return NULL
		},
	},
	"last": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1",
					len(args))
			}
			if args[0].Type() != object.ARRAY_OBJ {
				return newError("argument to `last` must be ARRAY, got %s",
					args[0].Type())
			}
			arr, ok := args[0].(*object.Array)
			if !ok {
				return newError("argument to `last` must be ARRAY, got %s", args[0].Type())
			}
			length := len(arr.Elements)
			if length > 0 {
				return arr.Elements[length-1]
			}
			return NULL
		},
	},
	"rest": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1",
					len(args))
			}
			if args[0].Type() != object.ARRAY_OBJ {
				return newError("argument to `rest` must be ARRAY, got %s",
					args[0].Type())
			}
			arr, ok := args[0].(*object.Array)
			if !ok {
				return newError("argument to `rest` must be ARRAY, got %s", args[0].Type())
			}
			length := len(arr.Elements)
			if length > 0 {
				newElements := make([]object.Object, length-1)
				copy(newElements, arr.Elements[1:length])
				return &object.Array{Elements: newElements}
			}
			return NULL
		},
	},
	"push": &object.Builtin{Fn: func(args ...object.Object) object.Object {
		if len(args) != 2 {
			return newError("wrong number of arguments. got=%d, want=2",
				len(args))
		}
		if args[0].Type() != object.ARRAY_OBJ {
			return newError("argument to `push` must be ARRAY, got %s",
				args[0].Type())
		}
		arr, ok := args[0].(*object.Array)
		if !ok {
			return newError("argument to `push` must be ARRAY, got %s", args[0].Type())
		}
		length := len(arr.Elements)
		newElements := make([]object.Object, length+1)
		copy(newElements, arr.Elements)
		newElements[length] = args[1]
		return &object.Array{Elements: newElements}
	}},
	// This is terrible design, going down the Javascript route here... p treats arrays as immutable, but pop does not...
	// Good thing it's but toy language
	"pop": &object.Builtin{Fn: func(args ...object.Object) object.Object {
		if len(args) != 1 {
			return newError("wrong number of arguments. got=%d, want=2",
				len(args))
		}
		if args[0].Type() != object.ARRAY_OBJ {
			return newError("argument to `push` must be ARRAY, got %s",
				args[0].Type())
		}
		arr, ok := args[0].(*object.Array)
		if !ok {
			return newError("argument to `push` must be ARRAY, got %s", args[0].Type())
		}
		arrayLen := len(arr.Elements)
		if arrayLen > 0 {
			last := arr.Elements[arrayLen-1]
			arr.Elements = arr.Elements[:arrayLen-1]
			switch obj := last.(type) {
			case *object.Integer:
				return &object.Integer{Value: obj.Value}
			case *object.Boolean:
				return &object.Boolean{Value: obj.Value}
			case *object.Null:
				return &object.Null{}
			case *object.Error:
				return &object.Error{Message: obj.Message}
			case *object.Function:
				return &object.Function{
					Parameters: obj.Parameters,
					Body:       obj.Body,
					Env:        obj.Env,
				}
			case *object.String:
				return &object.String{Value: obj.Value}
			case *object.Builtin:
				return &object.Builtin{Fn: obj.Fn}
			case *object.Array:
				return &object.Array{Elements: arr.Elements}
			default:
				return newError("array contains an unknown type %s", obj.Type())
			}
		} else {
			return newError("can't call pop on an array with %d elements", arrayLen)
		}
	}},
	"puts": &object.Builtin{Fn: func(args ...object.Object) object.Object {
		for _, arg := range args {
			fmt.Println(arg.Inspect())
		}
		return NULL
	}},
}
