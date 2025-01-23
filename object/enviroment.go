package object

// Environment is a classic name in interpreters, especially "lisp-y" ones.
// In reality you can think of it as a wrapper around a map[string]object.Object.
// It associates strings with evaluated objects.
type Environment struct {
	store map[string]Object // store is a map[string]object.Object of bound object to a given name
	outer *Environment
}

// NewEnvironment initializes a new Environment NOT enclosed (Environment.outer is nil!) structure and returns a pointer to it.
// It, of course, also takes care of Environment.store map initialization.
// If you want to create a new environment with containing enclosed environment you should call NewEnclosedEnvironment
// instead!
func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s, outer: nil}
}

// NewEnclosedEnvironment creates a new pointer to Environment where the outer parameter is the Environment.outer (enclosed)
// environment.
func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer
	return env
}

// Get gets a a given name from Environment store map.
// It returns Object and a bool, which represent weather a Object exists under the given name in the Environment or if
// default value was returned (there is no set value under that name).
// Note the Get also check all the enclosed Environment's before returning false as the found return value!
func (e *Environment) Get(name string) (object Object, found bool) {
	obj, ok := e.store[name]
	if !ok && e.outer != nil {
		obj, ok = e.outer.Get(name)
	}
	return obj, ok
}

// Set sets a val Object to a given name in the Environment store map.
// It returns back out the val Object.
func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val
	return val
}
