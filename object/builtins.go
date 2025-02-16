package object

import (
	"fmt"
	"time"
	"unsafe"
)

// Builtins is a slice of structs which holds builtin functions and associates each *Builtin with a name.
var Builtins = []struct {
	Name    string   // Name is a name for this builtin
	Builtin *Builtin //Builtin is the object systems representation of a builtin function/object
}{
	{
		"len",
		&Builtin{
			Fn: func(args ...Object) Object {
				if len(args) != 1 {
					return newError("wrong number of arguments. got=%d, want=1",
						len(args))
				}
				switch arg := args[0].(type) {
				case *String:
					return &Integer{Value: int64(len(arg.Value))}
				case *Array:
					return &Integer{Value: int64(len(arg.Elements))}
				default:
					return newError("argument to `len` not supported, got %s",
						args[0].Type())
				}
			},
		},
	},
	{
		"puts",
		&Builtin{Fn: func(args ...Object) Object {
			for _, arg := range args {
				fmt.Println(arg.Inspect())
			}
			return nil
		}},
	},
	{
		"first",
		&Builtin{
			Fn: func(args ...Object) Object {
				if len(args) != 1 {
					return newError("wrong number of arguments. got=%d, want=1",
						len(args))
				}
				if args[0].Type() != ARRAY_OBJ {
					return newError("argument to `first` must be ARRAY, got %s",
						args[0].Type())
				}
				arr, ok := args[0].(*Array)
				if !ok {
					return newError("argument to `first` must be ARRAY, got %s", args[0].Type())
				}
				if len(arr.Elements) > 0 {
					return arr.Elements[0]
				}
				return nil
			},
		},
	},
	{
		"last",
		&Builtin{
			Fn: func(args ...Object) Object {
				if len(args) != 1 {
					return newError("wrong number of arguments. got=%d, want=1",
						len(args))
				}
				if args[0].Type() != ARRAY_OBJ {
					return newError("argument to `last` must be ARRAY, got %s",
						args[0].Type())
				}
				arr, ok := args[0].(*Array)
				if !ok {
					return newError("argument to `last` must be ARRAY, got %s", args[0].Type())
				}
				length := len(arr.Elements)
				if length > 0 {
					return arr.Elements[length-1]
				}
				return nil
			},
		},
	},
	{
		"rest",
		&Builtin{
			Fn: func(args ...Object) Object {
				if len(args) != 1 {
					return newError("wrong number of arguments. got=%d, want=1",
						len(args))
				}
				if args[0].Type() != ARRAY_OBJ {
					return newError("argument to `rest` must be ARRAY, got %s",
						args[0].Type())
				}
				arr, ok := args[0].(*Array)
				if !ok {
					return newError("argument to `rest` must be ARRAY, got %s", args[0].Type())
				}
				length := len(arr.Elements)
				if length > 0 {
					newElements := make([]Object, length-1)
					copy(newElements, arr.Elements[1:length])
					return &Array{Elements: newElements}
				}
				return nil
			},
		},
	},
	{
		"push",
		&Builtin{Fn: func(args ...Object) Object {
			if len(args) != 2 {
				return newError("wrong number of arguments. got=%d, want=2",
					len(args))
			}
			if args[0].Type() != ARRAY_OBJ {
				return newError("argument to `push` must be ARRAY, got %s",
					args[0].Type())
			}
			arr, ok := args[0].(*Array)
			if !ok {
				return newError("argument to `push` must be ARRAY, got %s", args[0].Type())
			}
			length := len(arr.Elements)
			newElements := make([]Object, length+1)
			copy(newElements, arr.Elements)
			newElements[length] = args[1]
			return &Array{Elements: newElements}
		}},
	},
	{
		"now",
		&Builtin{
			Fn: func(args ...Object) Object {
				switch len(args) {
				case 0:
					return &Integer{Value: time.Now().Unix()}
				case 1:
					if formatArgument := args[0]; formatArgument.Type() == STRING_OBJ {
						stringObject, ok := formatArgument.(*String)
						if !ok {
							return newError("argument to `now` not supported, got %s", formatArgument.Type())
						}
						return &String{Value: time.Now().Format(stringObject.Value)}
					} else {
						return newError("wrong type of argument. got=%s, want=STRING", args[0].Type())
					}
				default:
					return newError("wrong number of arguments. got=%d, want=1 (format) or 0 for (unix epoch)", len(args))
				}
			},
		},
	},
	{
		"pop",
		&Builtin{Fn: func(args ...Object) Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=2",
					len(args))
			}
			if args[0].Type() != ARRAY_OBJ {
				return newError("argument to `push` must be ARRAY, got %s",
					args[0].Type())
			}
			arr, ok := args[0].(*Array)
			if !ok {
				return newError("argument to `push` must be ARRAY, got %s", args[0].Type())
			}
			arrayLen := len(arr.Elements)
			if arrayLen > 0 {
				last := arr.Elements[arrayLen-1]
				arr.Elements = arr.Elements[:arrayLen-1]
				switch obj := last.(type) {
				case *Integer:
					return &Integer{Value: obj.Value}
				case *Boolean:
					return &Boolean{Value: obj.Value}
				case *Error:
					return &Error{Message: obj.Message}
				case *Function:
					return &Function{
						Parameters: obj.Parameters,
						Body:       obj.Body,
						Env:        obj.Env,
					}
				case *String:
					return &String{Value: obj.Value}
				case *Builtin:
					return &Builtin{Fn: obj.Fn}
				case *Array:
					return &Array{Elements: arr.Elements}
				default:
					return newError("array contains an unknown type %s", obj.Type())
				}
			} else {
				return newError("can't call pop on an array with %d elements", arrayLen)
			}
		}},
	},
	{
		"size_of",
		&Builtin{
			Fn: func(args ...Object) Object {
				if len(args) != 1 {
					return newError("wrong number of arguments. got=%d, want=1", len(args))
				}
				switch arg := args[0].(type) {
				case *Integer:
					return &Integer{Value: int64(unsafe.Sizeof(arg.Value))}
				case *Boolean:
					return &Integer{Value: int64(unsafe.Sizeof(arg.Value))}
				case *ReturnValue:
					return &Integer{Value: int64(unsafe.Sizeof(arg.Value))}
				case *Error:
					return &Integer{Value: int64(unsafe.Sizeof(arg.Message))}
				case *Function:
					return &Integer{Value: int64(unsafe.Sizeof(arg.Body) + unsafe.Sizeof(arg.Parameters))}
				case *String:
					return &Integer{Value: int64(unsafe.Sizeof(arg.Value))}
				case *Builtin:
					return &Integer{Value: int64(unsafe.Sizeof(arg.Fn))}
				case *Array:
					return &Integer{Value: int64(unsafe.Sizeof(arg.Elements))}

				}
				return &Integer{Value: int64(unsafe.Sizeof(args[0]))}
			},
		},
	},
	{
		Name: "type_of",
		Builtin: &Builtin{
			Fn: func(args ...Object) Object {
				if len(args) != 1 {
					return newError("wrong number of arguments. got=%d, want=1", len(args))
				}
				return &String{Value: string(args[0].Type())}
			},
		},
	},
}

// GetBuiltinByName tries the find the name in Builtins slice. If it finds it, it returns a pointer to *Builtin.
// If it does not, a nil is returned to the caller.
func GetBuiltinByName(name string) *Builtin {
	for _, def := range Builtins {
		if def.Name == name {
			return def.Builtin
		}
	}
	return nil
}

// newError creates a new Error with the specified format string. It returns pointer to this new *Error.
func newError(format string, a ...interface{}) *Error {
	return &Error{Message: fmt.Sprintf(format, a...)}
}
